{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module RayTracer.Data.Pattern.StripePattern
  ( StripePattern (StripePattern, transformation, a, b),
    stripePattern,
  )
where

import Data.Fixed
import RayTracer.Data.Color
import RayTracer.Data.Matrix
import qualified RayTracer.Data.Pattern as P
import RayTracer.Data.Tuple
import RayTracer.Transformation

data StripePattern a = StripePattern
  { transformation :: !(Matrix a),
    a :: !(Color a),
    b :: !(Color a)
  }
  deriving (Eq, Show)

stripePattern :: RealFrac a => Color a -> Color a -> StripePattern a
stripePattern = StripePattern identity

instance RealFrac a => P.Pattern StripePattern a where
  transformation = transformation
  patternAt (StripePattern _ a b) (Tuple x _ _ _)
    | x `mod'` 2 < 1 = a
    | otherwise = b
