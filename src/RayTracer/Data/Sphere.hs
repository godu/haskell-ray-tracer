{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module RayTracer.Data.Sphere
  ( Sphere (origin, transformation, material),
    sphere,
  )
where

import RayTracer.Data.Intersection
  ( intersection,
    intersections,
  )
import qualified RayTracer.Data.Material as M
  ( Material,
    material,
  )
import RayTracer.Data.Matrix
  ( Matrix,
  )
import RayTracer.Data.Ray
  ( Ray (Ray),
  )
import qualified RayTracer.Data.Shape as S
  ( Shape
      ( localIntersect,
        localNormalAt,
        material,
        transformation
      ),
  )
import RayTracer.Data.Tuple
  ( Tuple,
    point,
    (.^),
  )
import RayTracer.Transformation (identity)
import Prelude
  ( Eq,
    Floating,
    Fractional,
    Num,
    Ord,
    Show,
    otherwise,
    sqrt,
    (*),
    (+),
    (-),
    (/),
    (<),
  )

data Sphere a = Sphere
  { origin :: !(Tuple a),
    transformation :: !(Matrix a),
    material :: !(M.Material a)
  }
  deriving (Show, Eq)

sphere :: (Fractional a) => Sphere a
sphere = Sphere (point 0 0 0) identity M.material

instance (Num a, Floating a, Ord a) => S.Shape Sphere a where
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
