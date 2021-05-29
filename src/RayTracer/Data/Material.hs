module RayTracer.Data.Material
  ( Material (pattern, ambient, diffuse, specular, shininess),
    material,
    lighting,
  )
where

import qualified RayTracer.Data.Color as C
  ( Color,
    black,
    color,
    (*^),
  )
import RayTracer.Data.Light (Light (intensity, position))
import RayTracer.Data.Pattern (Pattern, colorPattern, stripeAt)
import RayTracer.Data.Tuple
  ( Tuple,
    normalize,
    reflect,
    (.^),
  )
import Prelude
  ( Bool,
    Eq,
    Floating,
    Fractional,
    Num ((*), (+), (-)),
    Ord ((<), (<=)),
    RealFrac (floor),
    Show,
    otherwise,
    ($),
    (^),
  )

data Material a = Material
  { pattern :: !(Pattern a),
    ambient :: !a,
    diffuse :: !a,
    specular :: !a,
    shininess :: !a
  }
  deriving (Show, Eq)

material :: Fractional a => Material a
material = Material pattern 0.1 0.9 0.9 200.0
  where
    pattern = colorPattern $ C.color 1 1 1

lighting ::
  (Floating a, Ord a, RealFrac a) =>
  Material a ->
  Light a ->
  Tuple a ->
  Tuple a ->
  Tuple a ->
  Bool ->
  C.Color a
lighting m light point eyev normalv inShadow = if inShadow then ambient else ambient + diffuse + specular
  where
    Material pattern_ ambient_ diffuse_ specular_ shininess_ = m
    color = pattern_ `stripeAt` point
    effectiveColor = color * intensity light
    lightv = normalize $ position light - point
    ambient = effectiveColor C.*^ ambient_
    lightDotNormal = lightv .^ normalv
    diffuse
      | lightDotNormal < 0 = C.black
      | otherwise = effectiveColor C.*^ diffuse_ C.*^ lightDotNormal
    reflectv = reflect (- lightv) normalv
    reflectDotEye = reflectv .^ eyev
    factor = reflectDotEye ^ floor shininess_
    specular
      | lightDotNormal < 0 = C.black
      | reflectDotEye <= 0 = C.black
      | otherwise = intensity light C.*^ specular_ C.*^ factor
