module Fetch (fetchInput) where

import Control.Monad.IO.Class
import Data.String
import Network.HTTP.Req

fetchInput :: Int -> IO ()
fetchInput day = runReq defaultHttpConfig $ do
    r <- req GET (https (fromString "www.google.com")) NoReqBody bsResponse mempty
    liftIO $ print (responseBody r)
