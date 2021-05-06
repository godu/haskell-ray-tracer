module RayTracer.Data.Intersection.Computations
  ( Computations (t, object, point, eyev, normalv),
    prepareComputations,
  )
where

import qualified RayTracer.Data.Intersection as I (Intersection (object, t))
import RayTracer.Data.Ray (Ray (direction), position)
import RayTracer.Data.Shape (Shape)
import RayTracer.Data.Sphere (Sphere, normalAt)
import RayTracer.Data.Tuple (Tuple)
import Prelude
  ( Eq,
    Num (negate),
    ($),
  )

data Computations a o = Computations
  { t :: a,
    object :: o a,
    point :: Tuple a,
    eyev :: Tuple a,
    normalv :: Tuple a
  }

prepareComputations :: (Eq a, Num a, Shape o a) => I.Intersection a o -> Ray a -> Computations a o
prepareComputations intersection ray =
  Computations
    { t = _t,
      object = _object,
      point = _point,
      eyev = _eyev,
      normalv = _normalv
    }
  where
    _t = I.t intersection
    _object = I.object intersection
    _point = position _t ray
    _eyev = negate $ direction ray
    _normalv = normalAt _object _point
