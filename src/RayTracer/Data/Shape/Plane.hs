module RayTracer.Data.Shape.Plane
  ( Plane (Plane, transformation, material),
  )
where

import RayTracer.Data.Intersection (intersection)
import RayTracer.Data.Material (Material)
import RayTracer.Data.Matrix (Matrix)
import RayTracer.Data.Pattern (Pattern)
import RayTracer.Data.Ray (Ray (Ray))
import qualified RayTracer.Data.Shape as S (Shape (..))
import RayTracer.Data.Tuple (Tuple (y), vector)
import RayTracer.Extra (epsilon)

data Plane p a = Plane
  { transformation :: !(Matrix a),
    material :: !(Material p a)
  }
  deriving (Show, Eq)

instance (Num a, Floating a, Ord a, Eq (p a), Pattern p a) => S.Shape Plane p a where
  transformation = transformation
  material = material
  localIntersect (Ray o d) p
    | abs (y d) < epsilon = []
    | otherwise = [intersection t p]
    where
      t = negate $ y o / y d
  localNormalAt _ _ = vector 0 1 0
