module Util.Maths (quadraticRoots, lcmList) where

quadraticRoots :: (Floating a) => a -> a -> a -> (a, a)
quadraticRoots a b c =
    ( (-b + discr) / (2 * a)
    , (-b - discr) / (2 * a)
    )
  where
    discr = sqrt ((b ** 2) - (4 * a * c))

lcmList :: (Integral a) => [a] -> a
lcmList (n1 : n2 : ns) = lcmList (lcm n1 n2 : ns)
lcmList [n] = n
lcmList [] = error "an empty list has no lcm"
