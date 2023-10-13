{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Server (runServer) where

import Config (Config (..))
import Control.Concurrent (forkIO, newEmptyMVar, putMVar, readMVar)
import Control.Concurrent.STM (atomically, modifyTVar, newTVarIO, readTVar, readTVarIO)
import Control.Exception (SomeException, bracketOnError, catch)
import Control.Monad.Reader (MonadIO (liftIO), forever)
import Data.Aeson (encode, encodeFile)
import Data.Text (Text, pack)
import Data.Text.Lazy (fromStrict)
import DataBase (DBConfig (dbFile), newPaper, readDB)
import Fmt (format)
import Log (LogT, defaultLogLevel, logInfo_, runLogT)
import Log.Logger (Logger)
import Network.Socket (Family (AF_INET), SockAddr (SockAddrInet), SocketType (Stream), bind, close, listen, socket, socketPort, tupleToHostAddress, withSocketsDo)
import Network.Wai (Request (rawPathInfo), requestMethod)
import System.FilePath ((</>))
import System.IO (hPrint, stderr)
import Web.Scotty.Trans (ScottyT, captureParam, defaultOptions, file, function, get, matchAny, next, raw, regex, request, scottySocketT, text)

dispatch :: Config -> ScottyT (LogT IO) ()
dispatch Config{..} = do
    matchAny (function $ const $ Just []) $ do
        r <- request
        logInfo_ $ format "{} {}" (show $ requestMethod r) (show $ rawPathInfo r)
        next
    get "/" $ file $ staticDir </> "index.html"
    get "/api/papers/length" $ do
        n <- runSTM $ do
            db <- readTVar dbT
            return $ length db
        text . fromStrict . pack . show $ n
    get "/api/papers/new" $ do
        paper <- liftIO newPaper
        runSTM $ modifyTVar dbT (paper :)
        saveFile
        raw $ encode paper
    get (regex "^/api(/.*)?") $ do
        r <- request
        logInfo_ $ format "Invalid api request: {} {}" (show $ requestMethod r) (show $ rawPathInfo r)
        text "API request not recognized"
    get (regex "^/.*") $ do
        path <- captureParam "0"
        file $ staticDir </> drop 1 path
  where
    runSTM a = liftIO $ atomically a
    saveFile = liftIO $ putMVar saveSignal ()

serverFailed :: Text -> (ScottyT (LogT IO)) ()
serverFailed msg = do
    matchAny (function $ const $ Just []) $ do
        r <- request
        logInfo_ $ format "{} {}" (show $ requestMethod r) (show $ rawPathInfo r)
        next
    get "/" $ text $ fromStrict msg

runServer :: FilePath -> Logger -> IO ()
runServer staticDir logger = withSocketsDo $ do
    bracketOnError newSocket close $ \sock -> do
        bind sock $ SockAddrInet 0 $ tupleToHostAddress (127, 0, 0, 1)
        listen sock 1024
        port <- socketPort sock
        hPrint stderr port
        runLog $ do
            logInfo_ $ format "Server will be at http://localhost:{}" (show port)
            logInfo_ $ format "Serving static files from '{}'" staticDir
        result <- catch (runLog $ Right <$> readDB) $ \e -> return $ Left (show (e :: SomeException))
        case result of
            Left err -> scottySocketT defaultOptions sock runLog (serverFailed $ pack err)
            Right (dbConfig, db) -> do
                dbT <- newTVarIO db
                saveSignal <- newEmptyMVar
                let config =
                        Config
                            { staticDir
                            , dbConfig
                            , dbT
                            , saveSignal
                            }
                _ <- forkIO $ writeDB config
                scottySocketT defaultOptions sock runLog (dispatch config)
  where
    newSocket = socket AF_INET Stream 0
    runLog = runLogT "main" logger defaultLogLevel
    writeDB Config{saveSignal, dbT, dbConfig} = forever $ do
        _ <- readMVar saveSignal
        db' <- readTVarIO dbT
        encodeFile (dbFile dbConfig) db'
