{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module DataBase (Paper (..), DataBase, readDB, DBConfig (..), newPaper) where

import Control.Exception (catch)
import Control.Exception.Base (throw)
import Control.Monad.Reader (lift)
import Data.Aeson (FromJSON, ToJSON, eitherDecodeStrict)
import Data.String (fromString)
import Data.Text (Text, unpack)
import Data.UUID (UUID)
import Data.UUID.V4 (nextRandom)
import Fmt (format)
import GHC.Generics (Generic)
import Log (LogT, logAttention_, logInfo_)
import Optics (makeFieldLabelsNoPrefix)
import System.Directory (createDirectoryIfMissing, getUserDocumentsDirectory)
import System.FilePath (takeDirectory, (</>))
import System.IO (readFile')
import System.IO.Error (isDoesNotExistError)

data Paper = Paper
    { name :: Text
    , uuid :: UUID
    , file :: [FilePath]
    , tags :: [Text]
    , cite :: Text
    , url :: Text
    , comments :: Text
    }
    deriving (Generic, Show)

instance ToJSON Paper
instance FromJSON Paper

type DataBase = [Paper]

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
            , file = []
            , tags = []
            , cite = ""
            , url = ""
            , comments = ""
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
                writeFile dbFile "[]"
                return ("[]", logM)
            | otherwise -> do
                let logM = logInfo_ $ format "Failed to read db file: {}" (show e)
                return ("", logM >> throw e)
    logM
    case eitherDecodeStrict (fromString db) of
        Right x -> do
            logInfo_ $ format "Loaded database: {}" (show dbFile)
            return (config, x)
        Left err -> do
            let msg = format "Failed to decode {}: {}" dbFile err
            logAttention_ msg
            error $ unpack msg
