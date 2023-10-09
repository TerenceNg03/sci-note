{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Network.Wai.Middleware.RequestLogger (logStdout)
import System.Environment (getArgs)
import System.FilePath ((</>))
import Web.Scotty (captureParam, file, get, middleware, regex, scotty, text)

main :: IO ()
main = do
    args <- getArgs
    let base = case length args of
            x
                | x >= 1 -> head args
                | otherwise -> "./ui/build"
    print args
    putStrLn $ "Static directory: " ++ base
    launchServer base

launchServer :: FilePath -> IO ()
launchServer base = scotty 3000 $ do
    middleware logStdout
    get "/" $ file $ base </> "index.html"
    get "/api/:name" $ do
        name <- captureParam "name"
        text name
    get (regex "^/.*") $ do
        path <- captureParam "0"
        file $ base </> drop 1 path
