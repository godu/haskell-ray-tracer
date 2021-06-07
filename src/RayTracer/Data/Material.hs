module RayTracer.Data.Material
  ( Material (Material, pattern_, ambient, diffuse, specular, shininess),
    material,
  )
where

import qualified RayTracer.Data.Color as C
  ( color,
  )
import RayTracer.Data.Pattern (Pattern, colorPattern)
import Prelude
  ( Eq,
    Fractional,
    Show,
    ($),
  )

data Material a = Material
  { pattern_ :: !(Pattern a),
    ambient :: !a,
    diffuse :: !a,
    specular :: !a,
    shininess :: !a
  }
  deriving (Show, Eq)

material :: Fractional a => Material a
material = Material pattern_ 0.1 0.9 0.9 200.0
  where
    pattern_ = colorPattern $ C.color 1 1 1
