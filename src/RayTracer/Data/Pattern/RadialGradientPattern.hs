{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module RayTracer.Data.Pattern.RadialGradientPattern
  ( RadialGradientPattern (RadialGradientPattern, transformation, a, b),
    radialGradientPattern,
  )
where

import RayTracer.Data.Color ((*^))
import RayTracer.Data.Matrix (Matrix)
import RayTracer.Data.Pattern as P (Pattern (..))
import qualified RayTracer.Data.Pattern.Extra as P
import RayTracer.Data.Tuple (Tuple (..))
import RayTracer.Transformation (identity)

data RadialGradientPattern p a = RadialGradientPattern
  { transformation :: !(Matrix a),
    a :: !(p a),
    b :: !(p a)
  }
  deriving (Eq, Show)

radialGradientPattern :: Num a => p a -> p a -> RadialGradientPattern p a
radialGradientPattern = RadialGradientPattern identity

instance (RealFrac a, Floating a, P.Pattern p a) => P.Pattern (RadialGradientPattern p) a where
  getTransformation = transformation
  setTransformation p t = p {transformation = t}
  patternAt (RadialGradientPattern _ a b) p@(Tuple x _ z _) =
    do
      a' <- P.patternAtPattern a p
      b' <- P.patternAtPattern b p
      let r = sqrt (x * x + z * z)
          distance = b' - a'
          fraction = r - fromIntegral (floor r)
      pure $
        if even $ floor r
          then a' + (distance *^ fraction)
          else b' + (distance *^ negate fraction)
