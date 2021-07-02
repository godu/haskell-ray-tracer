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

import Data.Maybe
import RayTracer.Data.Intersection
import RayTracer.Data.Material
import RayTracer.Data.Matrix
import qualified RayTracer.Data.Pattern as P
import RayTracer.Data.Ray
import qualified RayTracer.Data.Tuple as T

class (P.Pattern p a, Eq (o p a)) => Shape o p a where
  transformation :: (Shape o p a) => o p a -> Matrix a
  material :: (Shape o p a) => o p a -> Material p a
  intersect :: (Eq a, Fractional a, Shape o p a) => Ray a -> o p a -> [Intersection (o p) a]
  intersect r s = case localRay of
    Nothing -> []
    Just r -> localIntersect r s
    where
      localRay = fmap (`transform` r) $ inverse $ transformation s
  localIntersect :: (Shape o p a) => Ray a -> o p a -> [Intersection (o p) a]
  normalAt :: (Eq a, Floating a, Fractional a, Shape o p a) => o p a -> T.Tuple a -> T.Tuple a
  normalAt s p = T.normalize $ worldNormal {T.w = 0}
    where
      t = fromJust $ inverse (transformation s)
      localPoint = t *^ p
      localNormal = localNormalAt s localPoint
      worldNormal = transpose t *^ localNormal
  localNormalAt :: (Shape o p a) => o p a -> T.Tuple a -> T.Tuple a
