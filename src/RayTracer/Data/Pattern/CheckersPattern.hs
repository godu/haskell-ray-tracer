module RayTracer.Data.Pattern.CheckersPattern
  ( CheckersPattern (CheckersPattern, transformation, a, b),
    checkersPattern,
  )
where

import qualified RayTracer.Data.Color as C
import RayTracer.Data.Matrix
import qualified RayTracer.Data.Pattern as P
import qualified RayTracer.Data.Tuple as T
import RayTracer.Transformation

data CheckersPattern a = CheckersPattern
  { transformation :: !(Matrix a),
    a :: !(C.Color a),
    b :: !(C.Color a)
  }
  deriving (Eq, Show)

checkersPattern :: (Num a) => C.Color a -> C.Color a -> CheckersPattern a
checkersPattern = CheckersPattern identity

instance (RealFrac a) => P.Pattern CheckersPattern a where
  transformation = transformation
  patternAt (CheckersPattern _ a b) (T.Tuple x y z _) =
    if even (floor x + floor y + floor z)
      then a
      else b
