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

import Network.Socket (PortNumber)
import Optics (makeFieldLabelsNoPrefix)

data Config = Config
    { staticDir :: FilePath
    , portNumber :: PortNumber
    , dbDir :: FilePath
    , dbFile :: FilePath
    }

makeFieldLabelsNoPrefix ''Config
