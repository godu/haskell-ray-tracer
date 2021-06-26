module RayTracer.Data.Pattern.StripePattern
  ( StripePattern (StripePattern, transformation, a, b),
    stripePattern,
  )
where

import Data.Fixed (mod')
import RayTracer.Data.Matrix (Matrix)
import RayTracer.Data.Pattern (Pattern (..))
import RayTracer.Data.Pattern.Extra (patternAtPattern)
import RayTracer.Data.Tuple (Tuple (Tuple))
import RayTracer.Transformation (identity)

data StripePattern p a = StripePattern
  { transformation :: !(Matrix a),
    a :: !(p a),
    b :: !(p a)
  }
  deriving (Eq, Show)

stripePattern :: Num a => p a -> p a -> StripePattern p a
stripePattern = StripePattern identity

instance (RealFrac a, Pattern p a) => Pattern (StripePattern p) a where
  getTransformation = transformation
  setTransformation p t = p {transformation = t}
  patternAt (StripePattern _ a b) p@(Tuple x _ _ _) = patternAtPattern (if x `mod'` 2 < 1 then a else b) p
