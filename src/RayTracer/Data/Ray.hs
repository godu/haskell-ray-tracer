module RayTracer.Data.Ray
  ( Ray (Ray, origin, direction),
    ray,
    position,
    transform,
  )
where

import qualified RayTracer.Data.Matrix as M
import qualified RayTracer.Data.Tuple as T

data Ray a = Ray
  { origin :: !(T.Tuple a),
    direction :: !(T.Tuple a)
  }
  deriving (Show, Eq)

ray :: T.Tuple a -> T.Tuple a -> Ray a
ray = Ray

position :: Num a => a -> Ray a -> T.Tuple a
position t (Ray o d) = o + (d T.*^ t)

transform :: Num a => M.Matrix a -> Ray a -> Ray a
transform t (Ray o d) = Ray (t M.*^ o) (t M.*^ d)
