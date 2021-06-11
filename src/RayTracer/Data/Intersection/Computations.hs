module RayTracer.Data.Intersection.Computations
  ( Computations (t, object, point, eyev, normalv, inside, overPoint),
    prepareComputations,
  )
where

import qualified RayTracer.Data.Intersection as I
import RayTracer.Data.Ray
import RayTracer.Data.Shape
import qualified RayTracer.Data.Tuple as T
import RayTracer.Extra

data Computations o a = Computations
  { t :: !a,
    object :: !(o a),
    point :: !(T.Tuple a),
    eyev :: !(T.Tuple a),
    normalv :: !(T.Tuple a),
    inside :: !Bool,
    overPoint :: !(T.Tuple a)
  }
  deriving (Eq, Show)

prepareComputations :: (Eq a, Num a, Shape o p a, Ord a, Fractional a, Floating a) => I.Intersection (o p) a -> Ray a -> Computations (o p) a
prepareComputations intersection ray =
  Computations
    { t = _t,
      object = _object,
      point = _point,
      eyev = _eyev,
      normalv = __normalv,
      inside = _inside,
      overPoint = _overPoint
    }
  where
    _t = I.t intersection
    _object = I.object intersection
    _point = position _t ray
    _eyev = negate $ direction ray
    _normalv = normalAt _object _point
    _inside = (_normalv T..^ _eyev) < 0
    __normalv = if _inside then negate _normalv else _normalv
    _overPoint = _point + __normalv T.*^ epsilon
