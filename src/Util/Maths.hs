module Util.Maths (quadraticRoots) where

quadraticRoots :: (Floating a) => a -> a -> a -> (a, a)
quadraticRoots a b c =
    ( (-b + discr) / (2 * a)
    , (-b - discr) / (2 * a)
    )
  where
    discr = sqrt ((b ** 2) - (4 * a * c))
