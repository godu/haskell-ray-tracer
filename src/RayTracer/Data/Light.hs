module RayTracer.Data.Light
  ( Light (position, intensity),
    pointLight,
  )
where

import RayTracer.Data.Color (Color)
import RayTracer.Data.Tuple (Tuple)
import Prelude
  ( Eq,
    Show,
  )

data Light a = Light
  { position :: !(Tuple a),
    intensity :: !(Color a)
  }
  deriving (Show, Eq)

pointLight :: Tuple a -> Color a -> Light a
pointLight = Light
