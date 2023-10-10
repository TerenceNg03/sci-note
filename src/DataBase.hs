{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module DataBase (Paper (..), loadDb) where

import Config (Config (Config, dbFile))
import Control.Exception (catch)
import Control.Exception.Base (throw)
import Control.Monad.Reader (ReaderT, ask, lift)
import Data.Aeson (FromJSON, ToJSON, decode)
import Data.String (fromString)
import Data.Text (Text)
import Fmt (format)
import GHC.Generics (Generic)
import Log (LogT, logAttention_, logInfo_)
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory)
import System.IO (readFile')
import System.IO.Error (isDoesNotExistError)

data Paper = Paper
    { name :: Text
    , file :: Maybe FilePath
    , tags :: [Text]
    , cite :: Text
    , url :: Text
    , comments :: Text
    }
    deriving (Generic, Show)

instance ToJSON Paper
instance FromJSON Paper

loadDb :: ReaderT Config (LogT IO) [Paper]
loadDb = do
    Config{..} <- ask
    (db, logM) <- lift $ lift $ catch ((,return ()) <$> readFile' dbFile) $ \case
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
    case decode (fromString db) of
        Just x -> do
            logInfo_ $ format "Loaded database: {}" (show dbFile)
            return x
        Nothing -> do
            logAttention_ $ format "Unable to decode database: {}" dbFile
            error "Can not decode db.json"
