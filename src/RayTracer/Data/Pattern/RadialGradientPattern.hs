{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module RayTracer.Data.Pattern.RadialGradientPattern
  ( RadialGradientPattern (RadialGradientPattern, transformation, a, b),
    radialGradientPattern,
  )
where

import qualified RayTracer.Data.Color as C
import RayTracer.Data.Matrix
import qualified RayTracer.Data.Pattern as P
import qualified RayTracer.Data.Tuple as T
import RayTracer.Transformation

data RadialGradientPattern a = RadialGradientPattern
  { transformation :: !(Matrix a),
    a :: !(C.Color a),
    b :: !(C.Color a)
  }
  deriving (Eq, Show)

radialGradientPattern :: Num a => C.Color a -> C.Color a -> RadialGradientPattern a
radialGradientPattern = RadialGradientPattern identity

instance (RealFrac a, Floating a) => P.Pattern RadialGradientPattern a where
  transformation = transformation
  patternAt (RadialGradientPattern _ a b) (T.Tuple x _ z _) =
    if even $ floor r
      then a + (distance C.*^ fraction)
      else b + (distance C.*^ negate fraction)
    where
      r = sqrt (x * x + z * z)
      distance = b - a
      fraction = r - fromIntegral (floor r)
