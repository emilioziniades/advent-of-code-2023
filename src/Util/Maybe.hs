module Util.Maybe (unwrap) where

unwrap :: Maybe a -> a
unwrap (Just x) = x
unwrap Nothing = error "unwrapped nothing"
