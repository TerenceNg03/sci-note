{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Config (Config (..))
import Control.Exception (SomeException, catch)
import Data.Aeson (encode, object, (.=))
import qualified Data.ByteString.Lazy as B
import Fmt (format)
import Log.Backend.StandardOutput.Bulk (withBulkStdOutLogger)
import Server (runServer)
import System.Console.CmdArgs (Data, Typeable, cmdArgs, def, opt, (&=))
import System.Directory (getUserDocumentsDirectory)
import System.FilePath ((</>))
import System.IO (hPutStrLn, stderr)

data Opts = Opts
    { dir :: FilePath
    , port :: Int
    }
    deriving (Typeable, Data)

opts :: Opts
opts =
    Opts
        { dir = def &= opt @String "./ui/build/"
        , port = def &= opt @Int 0
        }

buildConfig :: IO Config
buildConfig = do
    args <- cmdArgs opts
    docDir <- getUserDocumentsDirectory
    return
        Config
            { staticDir = dir args
            , portNumber = fromIntegral $ port args
            , dbDir = docDir </> "sci-note"
            , dbFile = docDir </> "sci-note/database.sqlite3"
            }

main :: IO ()
main = do
    config <- buildConfig
    catch
        (withBulkStdOutLogger $ runServer config)
        $ \e -> do
            B.hPutStr stderr $
                encode $
                    object
                        [ "error" .= True
                        , "title" .= ("Application terminated" :: String)
                        , "detail" .= show @SomeException e
                        , "message" .= (format "Application is terminated. If this is caused by a corrupted database file, try to fix or delete (will ERASE ALL DATA) the Sqlite3 Database at {}." (show $ dbFile config) :: String)
                        ]
            hPutStrLn stderr ""
