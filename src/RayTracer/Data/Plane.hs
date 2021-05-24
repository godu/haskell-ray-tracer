{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module RayTracer.Data.Plane
  ( Plane (transformation, material),
    plane,
  )
where

import RayTracer.Data.Intersection (intersection)
import qualified RayTracer.Data.Material as M
  ( Material,
    material,
  )
import RayTracer.Data.Matrix
  ( Matrix,
  )
import RayTracer.Data.Ray (Ray (Ray))
import qualified RayTracer.Data.Shape as S
  ( Shape
      ( localIntersect,
        localNormalAt,
        material,
        transformation
      ),
  )
import RayTracer.Data.Tuple
  ( Tuple (y),
    vector,
  )
import RayTracer.Extra (epsilon)
import RayTracer.Transformation (identity)
import Prelude
  ( Eq,
    Floating,
    Fractional ((/)),
    Num (abs, negate),
    Ord ((<)),
    Show,
    otherwise,
    ($),
  )

data Plane a = Plane
  { transformation :: !(Matrix a),
    material :: !(M.Material a)
  }
  deriving (Show, Eq)

plane :: (Fractional a) => Plane a
plane = Plane identity M.material

instance (Num a, Floating a, Ord a) => S.Shape Plane a where
  transformation = transformation
  material = material
  localIntersect (Ray o d) p
    | abs (y d) < epsilon = []
    | otherwise = [intersection t p]
    where
      t = negate $ y o / y d
  localNormalAt _ _ = vector 0 1 0
