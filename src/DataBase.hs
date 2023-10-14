{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module DataBase (Paper (..), DataBase (..), readDB, DBConfig (..), newPaper, insertPaper, lookupUUID) where

import Control.Exception (catch)
import Control.Exception.Base (throw)
import Control.Monad.Reader (lift)
import Data.Aeson (FromJSON, ToJSON, eitherDecodeStrict)
import Data.Int (Int8)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.String (fromString)
import Data.Text (Text, unpack)
import Data.UUID (UUID, fromString)
import Data.UUID.V4 (nextRandom)
import Fmt (format)
import GHC.Generics (Generic)
import Log (LogT, logAttention_, logInfo_)
import Optics (makeFieldLabelsNoPrefix, (%~), (&), (^.))
import System.Directory (createDirectoryIfMissing, getUserDocumentsDirectory)
import System.FilePath (takeDirectory, (</>))
import System.IO (readFile')
import System.IO.Error (isDoesNotExistError)

data Paper = Paper
    { name :: Text
    , uuid :: UUID
    , file :: Map Text FilePath
    , tags :: [Text]
    , cite :: Text
    , url :: Text
    , notes :: Text
    }
    deriving (Generic, Show)

makeFieldLabelsNoPrefix ''Paper

instance ToJSON Paper
instance FromJSON Paper

data DataBase = DataBase
    { papers :: [Paper]
    , favorites :: [UUID]
    , tagList :: Map Text (Int8, Int8, Int8)
    }
    deriving (Generic, Show)

defaultDB :: String
defaultDB = "{\"papers\":[],\"favorites\":[],\"tagList\":{}}"

insertPaper :: Paper -> DataBase -> DataBase
insertPaper paper db =
    db & #papers %~ (paper :) & #tagList %~ insert newTags
  where
    newTags = filter (\t -> Map.member t $ db ^. #tagList) (paper ^. #tags)
    insert [] = id
    insert (x : xs) = Map.insert x (255, 255, 255) . insert xs

lookupUUID :: String -> DataBase -> Either Text Paper
lookupUUID uuid db = guardUUID $ \pid ->
    case filter (\p -> p ^. #uuid == pid) $ db ^. #papers of
        (x : _) -> Right x
        [] -> Left "Invalid uuid"
  where
    guardUUID f = case Data.UUID.fromString uuid of
        Just uid -> f uid
        Nothing -> Left "Invalid uuid"

instance ToJSON DataBase
instance FromJSON DataBase

makeFieldLabelsNoPrefix ''DataBase

data DBConfig = DBConfig
    { dbDir :: FilePath
    , dbFile :: FilePath
    }

makeFieldLabelsNoPrefix ''DBConfig

newPaper :: IO Paper
newPaper = do
    uuid <- nextRandom
    return
        Paper
            { name = "Untitled"
            , uuid
            , file = Map.empty
            , tags = []
            , cite = ""
            , url = ""
            , notes = ""
            }

buildDBConfig :: IO DBConfig
buildDBConfig = do
    docDir <- getUserDocumentsDirectory
    return
        DBConfig
            { dbDir = docDir </> "sci-note"
            , dbFile = docDir </> "sci-note/db.json"
            }

readDB :: (LogT IO) (DBConfig, DataBase)
readDB = do
    config@DBConfig{..} <- lift buildDBConfig
    (db, logM) <- lift $ catch ((,return ()) <$> readFile' dbFile) $ \case
        e
            | isDoesNotExistError e -> do
                let logM = logAttention_ $ format "Created db file: {}" dbFile
                createDirectoryIfMissing True $ takeDirectory dbFile
                writeFile dbFile defaultDB
                return (defaultDB, logM)
            | otherwise -> do
                let logM = logInfo_ $ format "Failed to read db file: {}" (show e)
                return ("", logM >> throw e)
    logM
    case eitherDecodeStrict (Data.String.fromString db) of
        Right x -> do
            logInfo_ $ format "Loaded database: {}" (show dbFile)
            return (config, x)
        Left err -> do
            let msg = format "Failed to decode {}: {}" dbFile err
            logAttention_ msg
            error $ unpack msg
