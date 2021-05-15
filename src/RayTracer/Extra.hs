module RayTracer.Extra
  ( (~=),
    epsilon,
  )
where

import Prelude
  ( Bool,
    Eq,
    Fractional,
    Ord,
    Show,
    abs,
    fromInteger,
    ($),
    (-),
    (/),
    (>=),
  )

epsilon :: Fractional a => a
epsilon = 0.0001

(~=) :: (Ord a, Fractional a) => a -> a -> Bool
a ~= b = (>=) epsilon $ abs $ a - b
