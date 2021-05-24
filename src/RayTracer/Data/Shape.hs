{-# LANGUAGE MultiParamTypeClasses #-}

module RayTracer.Data.Shape
  ( Shape
      ( material,
        intersect,
        localIntersect,
        transformation,
        normalAt,
        localNormalAt
      ),
  )
where

import Data.Maybe (fromJust)
import RayTracer.Data.Intersection (Intersection)
import RayTracer.Data.Material (Material)
import RayTracer.Data.Matrix (Matrix, inverse, transpose, (*^))
import RayTracer.Data.Ray (Ray, transform)
import RayTracer.Data.Tuple (Tuple (w), normalize)
import Prelude (Eq, Floating, Fractional, Functor (fmap), Maybe (Just, Nothing), ($))

class Shape o a where
  transformation :: (Shape o a) => o a -> Matrix a
  material :: (Shape o a) => o a -> Material a
  intersect :: (Eq a, Fractional a, Shape o a) => Ray a -> o a -> [Intersection a o]
  intersect r s = case localRay of
    Nothing -> []
    Just r -> localIntersect r s
    where
      localRay = fmap (`transform` r) $ inverse $ transformation s
  localIntersect :: (Shape o a) => Ray a -> o a -> [Intersection a o]
  normalAt :: (Eq a, Floating a, Fractional a, Shape o a) => o a -> Tuple a -> Tuple a
  normalAt s p = normalize $ worldNormal {w = 0}
    where
      t = fromJust $ inverse (transformation s)
      localPoint = t *^ p
      localNormal = localNormalAt s localPoint
      worldNormal = transpose t *^ localNormal
  localNormalAt :: (Shape o a) => o a -> Tuple a -> Tuple a
