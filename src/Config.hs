{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Config (Config (..)) where

import Control.Concurrent (MVar)
import Control.Concurrent.STM (TVar)
import DataBase (DBConfig, DataBase)
import Optics (makeFieldLabelsNoPrefix)

data Config = Config
    { dbConfig :: DBConfig
    , staticDir :: FilePath
    , dbT :: TVar DataBase
    , saveSignal :: MVar ()
    }

makeFieldLabelsNoPrefix ''Config
