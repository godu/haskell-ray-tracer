{-# LANGUAGE MultiParamTypeClasses #-}

module RayTracer.Data.Shape
  ( Shape
      ( material,
        intersect,
        transformation,
        normalAt
      ),
  )
where

import RayTracer.Data.Intersection (Intersection)
import RayTracer.Data.Material (Material)
import RayTracer.Data.Matrix (Matrix)
import RayTracer.Data.Ray (Ray)
import RayTracer.Data.Tuple (Tuple)

class Shape o a where
  transformation :: (Shape o a) => o a -> Matrix a
  material :: (Shape o a) => o a -> Material a
  intersect :: (Shape o a) => Ray a -> o a -> [Intersection a o]
  normalAt :: (Shape o a) => o a -> Tuple a -> Tuple a
