module RayTracer.Data.Shape.Sphere
  ( Sphere (Sphere, transformation, material),
  )
where

import RayTracer.Data.Intersection (intersection, intersections)
import RayTracer.Data.Material (Material)
import RayTracer.Data.Matrix (Matrix)
import RayTracer.Data.Pattern (Pattern)
import RayTracer.Data.Ray (Ray (Ray))
import qualified RayTracer.Data.Shape as S (Shape (..))
import RayTracer.Data.Tuple (point, (.^))

data Sphere p a = Sphere
  { transformation :: !(Matrix a),
    material :: !(Material p a)
  }
  deriving (Show, Eq)

instance (Num a, Floating a, Ord a, Pattern p a, Eq (p a)) => S.Shape Sphere p a where
  transformation = transformation
  material = material
  localIntersect r s
    | discriminant < 0 = intersections []
    | otherwise = intersections [intersection t1 s, intersection t2 s]
    where
      Ray o d = r
      sphereToRay = o - point 0 0 0
      a = d .^ d
      b = 2 * d .^ sphereToRay
      c = sphereToRay .^ sphereToRay - 1
      discriminant = b * b - 4 * a * c
      t1 = ((- b) - sqrt discriminant) / (2 * a)
      t2 = ((- b) + sqrt discriminant) / (2 * a)
  localNormalAt _ p = p - point 0 0 0
