{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Server (runServer) where

import Config (Config (..))
import Control.Exception (bracketOnError)
import Control.Monad.Trans (liftIO)
import Data.Aeson (encode, object, (.=))
import qualified Data.ByteString.Lazy as B
import Data.Text (pack)
import DataBase (getFavorites, getPaper, getTags, likePaper, migrateAll, newPaper, tagPaper)
import Database.Esqueleto.Experimental (unValue)
import Database.Persist.Sqlite (Entity (entityVal), fromSqlKey, runMigration, runSqlite)
import Fmt (format)
import Log (LogT, defaultLogLevel, logInfo_, runLogT)
import Log.Logger (Logger)
import Network.HTTP.Types (status400)
import Network.Socket (Family (AF_INET), SockAddr (SockAddrInet), SocketType (Stream), bind, close, listen, socket, socketPort, tupleToHostAddress, withSocketsDo)
import Network.Wai (Request (rawPathInfo), requestMethod)
import System.FilePath ((</>))
import System.IO (hPutStrLn, stderr)
import Web.Scotty.Trans (ScottyT, captureParam, defaultOptions, file, formParam, function, get, matchAny, next, post, queryParam, raiseStatus, raw, regex, request, scottySocketT)

dispatch :: Config -> ScottyT (LogT IO) ()
dispatch Config{..} = do
    matchAny (function $ const $ Just []) $ do
        r <- request
        logInfo_ $ format "{} {}" (show $ requestMethod r) (show $ rawPathInfo r)
        next
    post "/api/new" $ do
        pid <- runSql newPaper
        raw $ encode $ object ["id" .= fromSqlKey pid]
    get "/api/get" $ do
        pid <- queryParam "id"
        (paper, tags) <- runSql $ getPaper pid
        raw $
            encode $
                object
                    [ "paper" .= map entityVal (take 1 paper)
                    , "tags" .= map unValue tags
                    ]
    get "/api/favorites" $ do
        favorites <- runSql getFavorites
        raw $ encode $ flip map favorites $ \(a, b) ->
            object
                [ "id" .= fromSqlKey (unValue a)
                , "title" .= unValue b
                ]
    get "/api/tags" $ do
        tags <- runSql getTags
        raw $ encode $ map entityVal tags
    post "/api/tag" $ do
        pid <- formParam "id"
        tag <- formParam "tag"
        runSql $ tagPaper pid tag
    post "/api/favorite" $ do
        pid <- formParam "id"
        runSql $ likePaper pid
    get (regex "^/api(/.*)?") $ do
        r <- request
        let err = format "Invalid API Request: {} {}" (show $ requestMethod r) (show $ rawPathInfo r)
        logInfo_ err
        raiseStatus status400 "Invalid API request"
    get "/" $ file $ staticDir </> "index.html"
    get (regex "^/.*") $ do
        path <- captureParam "0"
        file $ staticDir </> drop 1 path
  where
    runSql = runSqlite $ pack dbFile

runServer :: Config -> Logger -> IO ()
runServer config@Config{..} logger = withSocketsDo $ bracketOnError newSocket close $ \sock -> do
    bind sock $ SockAddrInet portNumber $ tupleToHostAddress (127, 0, 0, 1)
    listen sock 1024
    port <- socketPort sock
    runLog $
        do
            runSqlite (pack dbFile) (runMigration migrateAll)
            liftIO $
                B.hPutStr stderr $
                    encode $
                        object
                            ["error" .= False, "port" .= show port]
            liftIO $ hPutStrLn stderr ""
            logInfo_ $ format "Server will be at http://localhost:{}" (show port)
            logInfo_ $ format "Serving static files from '{}'" staticDir
            scottySocketT defaultOptions sock runLog (dispatch config)
  where
    newSocket = socket AF_INET Stream 0
    runLog = runLogT "main" logger defaultLogLevel
