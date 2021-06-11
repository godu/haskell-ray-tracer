{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module RayTracer.Data.Shape.Plane
  ( Plane (Plane, transformation, material),
  )
where

import RayTracer.Data.Intersection
import qualified RayTracer.Data.Material as M
import RayTracer.Data.Matrix
import qualified RayTracer.Data.Pattern as P
import RayTracer.Data.Ray
import qualified RayTracer.Data.Shape as S
import RayTracer.Data.Tuple
import RayTracer.Extra

data Plane p a = Plane
  { transformation :: !(Matrix a),
    material :: !(M.Material p a)
  }
  deriving (Show, Eq)

instance (Num a, Floating a, Ord a, Eq (p a), P.Pattern p a) => S.Shape Plane p a where
  transformation = transformation
  material = material
  localIntersect (Ray o d) p
    | abs (y d) < epsilon = []
    | otherwise = [intersection t p]
    where
      t = negate $ y o / y d
  localNormalAt _ _ = vector 0 1 0
