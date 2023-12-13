module Util.Debug (traceMe) where

import Debug.Trace

traceMe :: (Show a) => a -> a
traceMe a = traceShow a a
