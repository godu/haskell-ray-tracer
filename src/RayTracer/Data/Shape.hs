{-# LANGUAGE MultiParamTypeClasses #-}

module RayTracer.Data.Shape
  ( Shape,
    normalAt,
    intersect,
  )
where

import RayTracer.Data.Intersection (Intersection)
import RayTracer.Data.Ray (Ray)
import RayTracer.Data.Tuple (Tuple)
import Prelude
  ( Eq,
    Floating,
    Fractional,
    Num,
    Ord,
  )

class Shape s where
  normalAt :: (Eq a, Fractional a, Floating a) => s a -> Tuple a -> Tuple a

  intersect ::
    (Num a, Floating a, Eq a, Ord a, Fractional a) =>
    Ray a ->
    s a ->
    [Intersection a s]
