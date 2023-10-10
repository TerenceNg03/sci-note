{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Server (runServer) where

import Config (Config (..))
import Control.Exception (bracketOnError)
import Control.Monad.Reader (MonadReader (ask), MonadTrans (lift), ReaderT (runReaderT))
import DataBase (loadDb)
import Fmt (format)
import Log (LogT, defaultLogLevel, logInfo_, runLogT)
import Log.Logger (Logger)
import Network.Socket (Family (AF_INET), SockAddr (SockAddrInet), SocketType (Stream), bind, close, listen, socket, socketPort, tupleToHostAddress, withSocketsDo)
import Network.Wai (Request (rawPathInfo), requestMethod)
import System.FilePath ((</>))
import System.IO (hPrint, stderr)
import Web.Scotty.Trans (ScottyT, captureParam, defaultOptions, file, function, get, matchAny, next, regex, request, scottySocketT, text)

server :: ReaderT Config (ScottyT (LogT IO)) ()
server = do
    Config{..} <- ask
    lift $ do
        matchAny (function $ const $ Just []) $ do
            r <- request
            logInfo_ $ format "{} {}" (show $ requestMethod r) (show $ rawPathInfo r)
            next
        get "/" $ file $ staticDir </> "index.html"
        get "/api/:name" $ do
            name <- captureParam "name"
            text name
        get (regex "^/.*") $ do
            path <- captureParam "0"
            file $ staticDir </> drop 1 path

runServer :: Config -> Logger -> IO ()
runServer config logger = withSocketsDo $ do
    bracketOnError newSocket close $ \sock -> do
        bind sock $ SockAddrInet 0 $ tupleToHostAddress (127, 0, 0, 1)
        listen sock 1024
        port <- socketPort sock
        hPrint stderr port
        let Config{..} = config
        runLog $ flip runReaderT config $ do
            papers <- loadDb
            logInfo_ $ format "Loaded {} papers" (length papers)
            logInfo_ $ format "Server will be at http://localhost:{}" (show port)
            logInfo_ $ format "Serving static files from '{}'" staticDir
        scottySocketT defaultOptions sock runLog (runReaderT server config)
  where
    newSocket = socket AF_INET Stream 0
    runLog = runLogT "main" logger defaultLogLevel
