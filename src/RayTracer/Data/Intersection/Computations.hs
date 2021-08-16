module RayTracer.Data.Intersection.Computations
  ( Computations (t, object, point, eyev, normalv, inside, overPoint, reflectv),
    prepareComputations,
  )
where

import qualified RayTracer.Data.Intersection as I (Intersection (..))
import RayTracer.Data.Ray (Ray (direction), position)
import RayTracer.Data.Shape (Shape (normalAt))
import RayTracer.Data.Tuple (Tuple, reflect, (*^), (.^))
import RayTracer.Extra (epsilon)

data Computations o a = Computations
  { t :: !a,
    object :: !(o a),
    point :: !(Tuple a),
    eyev :: !(Tuple a),
    normalv :: !(Tuple a),
    inside :: !Bool,
    overPoint :: !(Tuple a),
    reflectv :: !(Tuple a)
  }
  deriving (Eq, Show)

prepareComputations :: (Shape o p a, Ord a, Fractional a, Floating a) => I.Intersection (o p) a -> Ray a -> Computations (o p) a
prepareComputations intersection ray =
  Computations
    { t = _t,
      object = _object,
      point = _point,
      eyev = _eyev,
      normalv = __normalv,
      inside = _inside,
      overPoint = _overPoint,
      reflectv = _reflectv
    }
  where
    _t = I.t intersection
    _object = I.object intersection
    _point = position _t ray
    _eyev = negate $ direction ray
    _normalv = normalAt _object _point
    _inside = (_normalv .^ _eyev) < 0
    __normalv = if _inside then negate _normalv else _normalv
    _overPoint = _point + __normalv *^ epsilon
    _reflectv = reflect (direction ray) __normalv
