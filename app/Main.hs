{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Network.Wai.Middleware.RequestLogger (logStdout)
import System.FilePath ((</>))
import Web.Scotty (captureParam, file, get, middleware, regex, scotty, text)

main :: IO ()
main = scotty 3000 $ do
    middleware logStdout
    get "/" $ file "ui/build/index.html"
    get "/api/:name" $ do
        name <- captureParam "name"
        text name
    get (regex "^/.*") $ do
        path <- captureParam "0"
        file $ "ui/build" </> drop 1 path
