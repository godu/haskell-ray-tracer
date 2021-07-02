module RayTracer.Data.Pattern.RingPattern
  ( RingPattern (RingPattern, transformation, a, b),
    ringPattern,
  )
where

import qualified RayTracer.Data.Color as C
import RayTracer.Data.Matrix
import qualified RayTracer.Data.Pattern as P
import qualified RayTracer.Data.Tuple as T
import RayTracer.Transformation

data RingPattern a = RingPattern
  { transformation :: !(Matrix a),
    a :: !(C.Color a),
    b :: !(C.Color a)
  }
  deriving (Eq, Show)

ringPattern :: Num a => C.Color a -> C.Color a -> RingPattern a
ringPattern = RingPattern identity

instance (RealFrac a, Floating a) => P.Pattern RingPattern a where
  transformation = transformation
  patternAt (RingPattern _ a b) (T.Tuple x _ z _) =
    if even $ floor $ sqrt (x * x + z * z)
      then a
      else b
