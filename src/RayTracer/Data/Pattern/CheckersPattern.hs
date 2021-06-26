module RayTracer.Data.Pattern.CheckersPattern
  ( CheckersPattern (CheckersPattern, transformation, a, b),
    checkersPattern,
  )
where

import RayTracer.Data.Matrix (Matrix)
import RayTracer.Data.Pattern (Pattern (..))
import RayTracer.Data.Pattern.Extra (patternAtPattern)
import RayTracer.Data.Tuple (Tuple (Tuple))
import RayTracer.Transformation (identity)

data CheckersPattern p a = CheckersPattern
  { transformation :: !(Matrix a),
    a :: !(p a),
    b :: !(p a)
  }
  deriving (Eq, Show)

checkersPattern :: (Fractional a) => p a -> p a -> CheckersPattern p a
checkersPattern = CheckersPattern identity

instance (RealFrac a, Pattern p a) => Pattern (CheckersPattern p) a where
  getTransformation = transformation
  setTransformation p t = p {transformation = t}
  patternAt (CheckersPattern _ a b) p@(Tuple x y z _) =
    patternAtPattern
      (if even (floor x + floor y + floor z) then a else b)
      p
