{-# LANGUAGE OverloadedStrings #-}

module Util.Fetch (getInput) where

import Configuration.Dotenv
import Control.Monad
import Control.Monad.IO.Class
import qualified Data.ByteString as BS
import Data.String (IsString (fromString))
import Network.HTTP.Req
import System.Directory
import System.Environment (getEnv)
import Text.Printf

type Day = Int

getInput :: Day -> IO ()
getInput day = do
    fileExists <- doesFileExist (inputFilename day)
    unless fileExists (downloadInput day)

downloadInput :: Day -> IO ()
downloadInput day = runReq defaultHttpConfig $ do
    loadFile defaultConfig
    token <- liftIO $ getEnv "AOC_TOKEN"
    let headers = header "Cookie" $ fromString $ "session=" <> token
    let url = https "adventofcode.com" /: "2023" /: "day" /~ day /: "input"
    r <- req GET url NoReqBody bsResponse headers
    liftIO $ BS.writeFile (inputFilename day) (responseBody r)

inputFilename :: Day -> String
inputFilename day = "input/" <> printf "day_%02d.txt" day
