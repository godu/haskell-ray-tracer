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
import RayTracer.Data.Pattern (Pattern)
import RayTracer.Data.Ray (Ray, transform)
import RayTracer.Data.Tuple (Tuple (w), normalize)

class (Pattern p a, Eq (o p a)) => Shape o p a where
  transformation :: o p a -> Matrix a
  material :: o p a -> Material p a
  intersect :: (Eq a, Fractional a) => Ray a -> o p a -> [Intersection (o p) a]
  intersect r s = case localRay of
    Nothing -> []
    Just r -> localIntersect r s
    where
      localRay = fmap (`transform` r) $ inverse $ transformation s
  localIntersect :: Ray a -> o p a -> [Intersection (o p) a]
  normalAt :: (Eq a, Floating a) => o p a -> Tuple a -> Tuple a
  normalAt s p = normalize $ worldNormal {w = 0}
    where
      t = fromJust $ inverse (transformation s)
      localPoint = t *^ p
      localNormal = localNormalAt s localPoint
      worldNormal = transpose t *^ localNormal
  localNormalAt :: o p a -> Tuple a -> Tuple a
