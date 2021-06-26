module RayTracer.Data.Ray
  ( Ray (Ray, origin, direction),
    ray,
    position,
    transform,
  )
where

import RayTracer.Data.Matrix as M (Matrix, (*^))
import RayTracer.Data.Tuple as T (Tuple, (*^))

data Ray a = Ray
  { origin :: !(Tuple a),
    direction :: !(Tuple a)
  }
  deriving (Show, Eq)

ray :: Tuple a -> Tuple a -> Ray a
ray = Ray

position :: Num a => a -> Ray a -> Tuple a
position t (Ray o d) = o + (d T.*^ t)

transform :: Num a => Matrix a -> Ray a -> Ray a
transform t (Ray o d) = Ray (t M.*^ o) (t M.*^ d)
