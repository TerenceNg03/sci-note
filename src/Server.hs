{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Server (runServer) where

import Config (Config (..))
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader (ask), MonadTrans (lift), ReaderT (runReaderT))
import DataBase (loadDb)
import Fmt (format)
import Log (LogT, MonadLog, defaultLogLevel, logInfo_, runLogT)
import Log.Logger (Logger)
import Optics.Operators ((^.))
import System.FilePath ((</>))
import Web.Scotty.Trans (RoutePattern, ScottyT, captureParam, file, get, regex, scottyT, text, request)
import Web.Scotty.Internal.Types (ActionT)
import Network.Wai (requestMethod, Request (rawPathInfo))

getL :: (MonadUnliftIO m, MonadLog m) => RoutePattern -> ActionT m () -> ScottyT m ()
getL route action = get route $ do
    r <- request
    logInfo_ $ format "{} {}" (show $ requestMethod r) (show $ rawPathInfo r)
    action

server :: ReaderT Config (ScottyT (LogT IO)) ()
server = do
    Config{..} <- ask
    lift $ do
        getL "/" $ file $ staticDir </> "index.html"
        getL "/api/:name" $ do
            name <- captureParam "name"
            text name
        getL (regex "^/.*") $ do
            path <- captureParam "0"
            file $ staticDir </> drop 1 path

runServer :: Config -> Logger -> IO ()
runServer config logger = do
    let Config{..} = config
    runLogT "main" logger defaultLogLevel $ flip runReaderT config $ do
        papers <- loadDb
        logInfo_ $ format "Loaded {} papers" (length papers)
        logInfo_ $ format "Visit server at http://localhost:{}" port
        logInfo_ $ format "Serving static files from '{}'" staticDir
    scottyT (config ^. #port) (runLogT "main" logger defaultLogLevel) (runReaderT server config)
