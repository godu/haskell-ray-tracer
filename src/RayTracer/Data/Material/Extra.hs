module RayTracer.Data.Material.Extra
  ( lighting,
  )
where

import Data.Maybe (fromMaybe)
import qualified RayTracer.Data.Color as C
  ( Color,
    black,
    (*^),
  )
import RayTracer.Data.Light (Light (intensity, position))
import RayTracer.Data.Material (Material (Material))
import RayTracer.Data.Pattern (black)
import RayTracer.Data.Pattern.Extra (stripeAtObject)
import qualified RayTracer.Data.Shape as S
  ( Shape,
  )
import RayTracer.Data.Tuple
  ( Tuple,
    normalize,
    reflect,
    (.^),
  )
import Prelude
  ( Bool,
    Floating,
    Num ((*), (+), (-)),
    Ord ((<), (<=)),
    RealFrac (floor),
    otherwise,
    ($),
    (^),
  )

lighting ::
  (S.Shape o a, Floating a, Ord a, RealFrac a) =>
  Material a ->
  o a ->
  Light a ->
  Tuple a ->
  Tuple a ->
  Tuple a ->
  Bool ->
  C.Color a
lighting m object light point eyev normalv inShadow = if inShadow then ambient else ambient + diffuse + specular
  where
    Material pattern_ ambient_ diffuse_ specular_ shininess_ = m
    color = fromMaybe black $ stripeAtObject pattern_ object point
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
