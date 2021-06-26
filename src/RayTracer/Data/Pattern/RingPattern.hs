module RayTracer.Data.Pattern.RingPattern
  ( RingPattern (RingPattern, transformation, a, b),
    ringPattern,
  )
where

import RayTracer.Data.Matrix (Matrix)
import RayTracer.Data.Pattern (Pattern (..))
import RayTracer.Data.Pattern.Extra (patternAtPattern)
import RayTracer.Data.Tuple (Tuple (Tuple))
import RayTracer.Transformation (identity)

data RingPattern p a = RingPattern
  { transformation :: !(Matrix a),
    a :: !(p a),
    b :: !(p a)
  }
  deriving (Eq, Show)

ringPattern :: Num a => p a -> p a -> RingPattern p a
ringPattern = RingPattern identity

instance (RealFrac a, Floating a, Pattern p a) => Pattern (RingPattern p) a where
  getTransformation = transformation
  setTransformation p t = p {transformation = t}
  patternAt (RingPattern _ a b) p@(Tuple x _ z _) =
    patternAtPattern
      (if even $ floor $ sqrt (x * x + z * z) then a else b)
      p
