{-# LANGUAGE MultiParamTypeClasses #-}

module RayTracer.Data.Shape (Shape (intersect, normalAt)) where

import RayTracer.Data.Intersection (Intersection)
import RayTracer.Data.Ray (Ray)
import RayTracer.Data.Tuple (Tuple)

class Shape o a where
  intersect :: (Shape o a) => Ray a -> o a -> [Intersection a o]
  normalAt :: (Shape o a) => o a -> Tuple a -> Tuple a