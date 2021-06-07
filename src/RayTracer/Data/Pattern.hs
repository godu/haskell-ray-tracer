module RayTracer.Data.Pattern
  ( black,
    white,
    Pattern (ColorPattern, StripePattern, a, b, transformation),
    colorPattern,
    stripePattern,
  )
where

import RayTracer.Data.Color
  ( Color,
    black,
    white,
  )
import RayTracer.Data.Matrix (Matrix)
import RayTracer.Transformation (identity)
import Prelude
  ( Eq,
    Num,
    Show,
  )

data Pattern a
  = ColorPattern
      { transformation :: !(Matrix a),
        color :: !(Color a)
      }
  | StripePattern
      { transformation :: !(Matrix a),
        a :: !(Color a),
        b :: !(Color a)
      }
  deriving (Eq, Show)

colorPattern :: Num a => Color a -> Pattern a
colorPattern = ColorPattern identity

stripePattern :: Num a => Color a -> Color a -> Pattern a
stripePattern = StripePattern identity
