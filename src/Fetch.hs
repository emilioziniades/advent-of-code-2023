{-# LANGUAGE OverloadedStrings #-}

module Fetch (fetchInput) where

import Configuration.Dotenv
import Control.Monad.IO.Class
import qualified Data.ByteString as BS
import Data.String (IsString (fromString))
import Network.HTTP.Req
import System.Environment (getEnv)

type Day = Int

fetchInput :: Day -> IO ()
fetchInput day = runReq defaultHttpConfig $ do
    loadFile defaultConfig
    token <- liftIO $ getEnv "AOC_TOKEN"
    let headers = header "Cookie" $ fromString $ "session=" ++ token
    let url = https "adventofcode.com" /: "2022" /: "day" /~ day /: "input"
    r <- req GET url NoReqBody bsResponse headers
    liftIO $ BS.writeFile "hellyeah.txt" $ responseBody r
