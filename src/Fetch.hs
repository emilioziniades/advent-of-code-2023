{-# LANGUAGE OverloadedStrings #-}

module Fetch (fetchInput) where

import Configuration.Dotenv
import Control.Monad.IO.Class
import Data.String (IsString (fromString))
import Network.HTTP.Req
import System.Environment (getEnv)

fetchInput :: Int -> IO ()
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

    liftIO $ print (responseBody r)
