{-# LANGUAGE OverloadedStrings #-}

module Fetch (fetchInput) where

import Configuration.Dotenv
import Control.Monad.IO.Class
import qualified Data.ByteString as BS
import Data.String (IsString (fromString))
import Network.HTTP.Req
import System.Environment (getEnv)

type Day = Int

fetchInput :: Day -> IO String
fetchInput day = runReq defaultHttpConfig $ do
    loadFile defaultConfig
    token <- liftIO $ getEnv "AOC_TOKEN"
    r <-
        req
            GET
            (https "adventofcode.com" /: "2022" /: "day" /~ day /: "input")
            NoReqBody
            bsResponse
            (header "Cookie" $ fromString $ "session=" ++ token)
    BS.writeFile "test" $ responseBody r
    liftIO $ print (responseBody r)
