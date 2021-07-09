module RayTracer.Data.Pattern.ColorPattern
  ( ColorPattern (ColorPattern, transformation, color),
    colorPattern,
  )
where

import qualified RayTracer.Data.Color as C
import RayTracer.Data.Matrix
import qualified RayTracer.Data.Pattern as P
import RayTracer.Transformation

data ColorPattern a = ColorPattern
  { transformation :: !(Matrix a),
    color :: !(C.Color a)
  }
  deriving (Eq, Show)

colorPattern :: Num a => C.Color a -> ColorPattern a
colorPattern = ColorPattern identity

instance (Ord a, Fractional a) => P.Pattern ColorPattern a where
  transformation = transformation
  patternAt (ColorPattern _ a) _ = a
