module RayTracer.Data.Pattern.Extra
  ( stripeAt,
    stripeAtObject,
  )
where

import Data.Fixed (mod')
import RayTracer.Data.Color
  ( Color,
  )
import RayTracer.Data.Matrix (inverse, (*^))
import RayTracer.Data.Pattern (Pattern (ColorPattern, StripePattern, transformation))
import qualified RayTracer.Data.Shape as S (Shape (transformation))
import RayTracer.Data.Tuple
  ( Tuple (Tuple),
  )
import Prelude
  ( Applicative ((<*>)),
    Maybe,
    Num,
    RealFrac,
    otherwise,
    (<),
    (<$>),
  )

stripeAt :: (RealFrac a) => Pattern a -> Tuple a -> Color a
stripeAt (ColorPattern _ a) _ = a
stripeAt (StripePattern _ a b) (Tuple x _ _ _)
  | x `mod'` 2 < 1 = a
  | otherwise = b

stripeAtObject :: (S.Shape o a, Num a, RealFrac a) => Pattern a -> o a -> Tuple a -> Maybe (Color a)
stripeAtObject pattern_ object worldPoint = stripeAt pattern_ <$> patternPoint
  where
    objectPoint = (*^ worldPoint) <$> inverse (S.transformation object)
    patternPoint = (*^) <$> inverse (transformation pattern_) <*> objectPoint
