module RayTracer.Data.Pattern.ColorPattern
  ( ColorPattern (ColorPattern, transformation, color),
    colorPattern,
  )
where

import RayTracer.Data.Color (Color)
import RayTracer.Data.Matrix (Matrix)
import RayTracer.Data.Pattern (Pattern (..))
import RayTracer.Transformation (identity)

data ColorPattern a = ColorPattern
  { transformation :: !(Matrix a),
    color :: !(Color a)
  }
  deriving (Eq, Show)

colorPattern :: Num a => Color a -> ColorPattern a
colorPattern = ColorPattern identity

instance (Ord a, Fractional a) => Pattern ColorPattern a where
  getTransformation = transformation
  setTransformation p t = p {transformation = t}
  patternAt (ColorPattern _ a) _ = pure a
