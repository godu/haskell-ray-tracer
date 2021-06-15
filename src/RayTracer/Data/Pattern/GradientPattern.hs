{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module RayTracer.Data.Pattern.GradientPattern
  ( GradientPattern (GradientPattern, transformation, a, b),
    gradientPattern,
  )
where

import qualified RayTracer.Data.Color as C
import RayTracer.Data.Matrix
import qualified RayTracer.Data.Pattern as P
import qualified RayTracer.Data.Tuple as T
import RayTracer.Transformation

data GradientPattern a = GradientPattern
  { transformation :: !(Matrix a),
    a :: !(C.Color a),
    b :: !(C.Color a)
  }
  deriving (Eq, Show)

gradientPattern :: Num a => C.Color a -> C.Color a -> GradientPattern a
gradientPattern = GradientPattern identity

instance (RealFrac a) => P.Pattern GradientPattern a where
  transformation = transformation
  patternAt (GradientPattern _ a b) (T.Tuple x _ _ _) = a + (distance C.*^ fraction)
    where
      distance = b - a
      fraction = x - fromIntegral (floor x)
