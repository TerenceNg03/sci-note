{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Config (Config (..), getConfig) where

import Optics (makeFieldLabelsNoPrefix)
import System.Directory (getTemporaryDirectory, getUserDocumentsDirectory, makeAbsolute)
import System.FilePath ((</>))

data Config = Config
    { dbDir :: FilePath
    , logFile :: FilePath
    , dbFile :: FilePath
    , staticDir :: FilePath
    , port :: Int
    }

makeFieldLabelsNoPrefix ''Config

getConfig :: FilePath -> IO Config
getConfig staticDir' = do
    docDir <- getUserDocumentsDirectory
    logDir <- getTemporaryDirectory
    staticDir <- makeAbsolute staticDir'
    return
        Config
            { dbDir = docDir </> "sci-note"
            , logFile = logDir </> "sci-note.log"
            , dbFile = docDir </> "db.json"
            , staticDir
            , port = 3000
            }
