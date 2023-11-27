{-# LANGUAGE OverloadedStrings #-}

module Fetch (fetchInput) where

import Control.Monad.IO.Class
import Network.HTTP.Req

fetchInput :: Int -> IO ()
fetchInput day = runReq defaultHttpConfig $ do
    r <-
        req
            GET
            (https "adventofcode.com" /: "2022" /: "day" /~ day /: "input")
            NoReqBody
            bsResponse
            mempty
    liftIO $ print (responseBody r)
