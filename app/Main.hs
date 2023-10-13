{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Log.Backend.StandardOutput.Bulk (withBulkStdOutLogger)
import Server (runServer)
import System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs
    let staticDir =
            if not (null args)
                then head args
                else "./ui/build/"
    withBulkStdOutLogger $ runServer staticDir
