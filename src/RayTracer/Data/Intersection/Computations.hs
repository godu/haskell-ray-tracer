module RayTracer.Data.Intersection.Computations
  ( Computations (t, object, point, eyev, normalv, inside),
    prepareComputations,
  )
where

import qualified RayTracer.Data.Intersection as I (Intersection (object, t))
import RayTracer.Data.Ray (Ray (direction), position)
import RayTracer.Data.Shape (Shape)
import RayTracer.Data.Sphere (Sphere, normalAt)
import RayTracer.Data.Tuple (Tuple, (.^))
import Prelude
  ( Bool (True),
    Eq,
    Num (negate),
    Ord ((<)),
    ($),
  )

data Computations a o = Computations
  { t :: a,
    object :: o a,
    point :: Tuple a,
    eyev :: Tuple a,
    normalv :: Tuple a,
    inside :: Bool
  }

prepareComputations :: (Eq a, Num a, Shape o a, Ord a) => I.Intersection a o -> Ray a -> Computations a o
prepareComputations intersection ray =
  Computations
    { t = _t,
      object = _object,
      point = _point,
      eyev = _eyev,
      normalv = __normalv,
      inside = _inside
    }
  where
    _t = I.t intersection
    _object = I.object intersection
    _point = position _t ray
    _eyev = negate $ direction ray
    _normalv = normalAt _object _point
    _inside = (_normalv .^ _eyev) < 0
    __normalv = if _inside then negate _normalv else _normalv