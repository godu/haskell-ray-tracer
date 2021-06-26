module RayTracer.Data.Pattern.BlendedPattern
  ( BlendedPattern (BlendedPattern, transformation, a, b),
    blendedPattern,
  )
where

import RayTracer.Data.Matrix (Matrix)
import RayTracer.Data.Pattern (Pattern (..))
import RayTracer.Data.Pattern.Extra (patternAtPattern)
import RayTracer.Transformation (identity)

data BlendedPattern p a = BlendedPattern
  { transformation :: !(Matrix a),
    a :: !(p a),
    b :: !(p a)
  }
  deriving (Eq, Show)

blendedPattern :: (Fractional a) => p a -> p a -> BlendedPattern p a
blendedPattern = BlendedPattern identity

instance (RealFrac a, Pattern p a) => Pattern (BlendedPattern p) a where
  getTransformation = transformation
  setTransformation p t = p {transformation = t}
  patternAt (BlendedPattern _ a b) p = do
    a' <- patternAtPattern a p
    b' <- patternAtPattern b p
    pure $ (/ 2) <$> (a' + b')
