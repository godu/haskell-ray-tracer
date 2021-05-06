{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module RayTracer.Data.Sphere
  ( Sphere (origin, transformation, material),
    sphere,
    intersect,
    normalAt,
  )
where

import Data.Maybe
  ( Maybe,
    fromJust,
    maybe,
  )
import RayTracer.Data.Intersection
  ( Intersection,
    intersection,
    intersections,
  )
import qualified RayTracer.Data.Material as M
  ( Material,
    material,
  )
import RayTracer.Data.Matrix
  ( Matrix,
    inverse,
    transpose,
    (*^),
  )
import RayTracer.Data.Ray
  ( Ray (Ray),
    transform,
  )
import RayTracer.Data.Shape (Shape (intersect, normalAt))
import RayTracer.Data.Tuple
  ( Tuple (w),
    normalize,
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
    fmap,
    otherwise,
    sqrt,
    ($),
    (*),
    (+),
    (-),
    (/),
    (<),
    (<$>),
    (<*>),
  )

data Sphere a = Sphere
  { origin :: Tuple a,
    transformation :: Matrix a,
    material :: M.Material a
  }
  deriving (Show, Eq)

sphere :: (Fractional a) => Sphere a
sphere = Sphere (point 0 0 0) identity M.material

instance (Num a, Floating a, Ord a) => Shape Sphere a where
  intersect r s = maybe [] (`intersect_` s) r2
    where
      r2 = fmap (`transform` r) $ inverse $ transformation s
      intersect_ r s
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

  normalAt s worldPoint = normalize $ worldNormal {w = 0}
    where
      t = fromJust $ inverse (transformation s)
      objectPoint = t *^ worldPoint
      objectNormal = objectPoint - point 0 0 0
      worldNormal = transpose t *^ objectNormal
