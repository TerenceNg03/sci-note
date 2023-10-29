{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module DataBase (migrateAll, newPaper, getPaper, getTags, getFavorites, tagPaper, likePaper) where

import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO)
import Data.Int (Int64)
import Data.Text (Text)
import Data.Word (Word8)
import Database.Esqueleto.Experimental (Entity, PersistEntity (Key), SqlReadT, Value, entityKey, from, innerJoin, insert, on, select, table, toSqlKey, val, where_, (==.), (^.), type (:&) ((:&)))
import Database.Persist.Sql (SqlPersistT)
import Database.Persist.TH (mkMigrate, mkPersist, persistLowerCase, share, sqlSettings)
import GHC.Generics (Generic)

share
    [mkPersist sqlSettings, mkMigrate "migrateAll"]
    [persistLowerCase| 
Paper json
    title Text
    author Text
    file (Maybe FilePath)
    cite Text
    url Text
    notes Text

    deriving Show 
Tag json
    name Text
    red Word8
    green Word8
    blue Word8
    alpha Word8

    Primary name

    deriving Show Generic
TagMap
    tag TagId
    paper PaperId

    Primary paper

    deriving Show
Favorite json
    paper PaperId

    Primary paper
    deriving Show
|]

newPaper :: (MonadIO m) => SqlPersistT m PaperId
newPaper =
    insert $
        Paper
            "Untitled"
            "<author>"
            Nothing
            "<cite>"
            "<paper url>"
            "# Notes"

getPaper :: (MonadIO m) => Int64 -> SqlReadT m ([Entity Paper], [Value Text])
getPaper pid = do
    tags <- select $ do
        (a :& b) <-
            from
                $ table @TagMap
                    `innerJoin` table @Tag
                `on` do \(tm :& t) -> tm.tag ==. t ^. TagId
        where_ (a.paper ==. val (toSqlKey pid))
        return b.name
    paper <- select $ do
        paper <- from $ table @Paper
        where_ (paper.id ==. val (toSqlKey pid))
        return paper
    return (paper, tags)

getTags :: (MonadIO m) => SqlReadT m [Entity Tag]
getTags = select $ from $ table @Tag

getFavorites :: (MonadIO m) => SqlReadT m [(Value PaperId, Value Text)]
getFavorites = select $ do
    (a :& b) <-
        from
            $ table @Favorite
                `innerJoin` table @Paper
            `on` do \(f :& p) -> f.paper ==. p ^. PaperId
    return (a.paper, b.title)

getTagId :: (MonadIO m) => Text -> SqlPersistT m (Key Tag)
getTagId name = do
    tagId <- select $ do
        tag <- from $ table @Tag
        where_ (tag.name ==. val name)
        return tag
    case tagId of
        (x : _) -> return $ entityKey x
        [] -> insert $ Tag name 255 255 255 255

tagPaper :: (MonadIO m) => Int64 -> Text -> SqlPersistT m ()
tagPaper pid name = void $ do
    tag <- getTagId name
    insert $ TagMap tag (toSqlKey pid)

likePaper :: (MonadIO m) => Int64 -> SqlPersistT m ()
likePaper pid = void $ do
    insert $ Favorite (toSqlKey pid)
