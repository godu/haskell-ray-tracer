module RayTracer.Data.Pattern
  ( black,
    white,
    Pattern (a, b),
    colorPattern,
    stripePattern,
    stripeAt,
  )
where

import Data.Fixed (mod')
import RayTracer.Data.Color
  ( Color,
    black,
    white,
  )
import RayTracer.Data.Tuple
  ( Tuple (Tuple),
  )
import Prelude
  ( Eq,
    RealFrac,
    Show,
    otherwise,
    (<),
  )

data Pattern a
  = ColorPattern !(Color a)
  | StripePattern
      { a :: !(Color a),
        b :: !(Color a)
      }
  deriving (Eq, Show)

colorPattern :: Color a -> Pattern a
colorPattern = ColorPattern

stripePattern :: Color a -> Color a -> Pattern a
stripePattern = StripePattern

stripeAt :: (RealFrac a) => Pattern a -> Tuple a -> Color a
stripeAt (ColorPattern a) _ = a
stripeAt (StripePattern a b) (Tuple x _ _ _)
  | x `mod'` 2 < 1 = a
  | otherwise = b
