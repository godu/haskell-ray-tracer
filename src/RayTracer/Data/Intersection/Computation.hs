module RayTracer.Data.Intersection.Computation
  ( Computation (t, object, point, eyev, normalv, inside),
    computation,
    prepareComputations,
  )
where

import Data.List
  ( find,
    sortOn,
  )
import Data.Maybe (Maybe (Nothing))
import qualified RayTracer.Data.Intersection as I (Intersection (object, t))
import qualified RayTracer.Data.Ray as R (Ray, direction, position)
import RayTracer.Data.Shape (Shape, normalAt)
import RayTracer.Data.Tuple (Tuple, (.^))
import Prelude
  ( Bool,
    Eq,
    Floating,
    Fractional,
    Num,
    Ord,
    Show,
    id,
    (.),
    (<),
    (>),
  )

data Computation a o = Computation
  { t :: a,
    object :: o a,
    point :: Tuple a,
    eyev :: Tuple a,
    normalv :: Tuple a,
    inside :: Bool
  }
  deriving (Show, Eq)

computation :: a -> o a -> Tuple a -> Tuple a -> Tuple a -> Bool -> Computation a o
computation = Computation

prepareComputations ::
  (Shape o, Fractional a, Eq a, Floating a, Ord a) =>
  I.Intersection a o ->
  R.Ray a ->
  Computation a o
prepareComputations i r = computation t_ o_ p_ e_ n__ i_
  where
    t_ = I.t i
    o_ = I.object i
    p_ = R.position t_ r
    e_ = - (R.direction r)
    n_ = o_ `normalAt` p_
    i_ = (n_ .^ e_) < 0
    n__ = if i_ then - n_ else n_
