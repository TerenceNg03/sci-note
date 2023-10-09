{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Config (getConfig)
import Server (runServer)
import System.Environment (getArgs)
import Log.Backend.StandardOutput.Bulk (withBulkStdOutLogger)

main :: IO ()
main = do
    args <- getArgs
    let staticDir =
            if not (null args)
                then head args
                else "./ui/build/"
    config <- getConfig staticDir
    withBulkStdOutLogger $ runServer config
