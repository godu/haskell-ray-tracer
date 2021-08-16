module RayTracer.Data.Material.Extra
  ( lighting,
  )
where

import Data.Maybe (fromMaybe)
import RayTracer.Data.Color (Color, black, (*^))
import RayTracer.Data.Light (Light (..))
import RayTracer.Data.Material (Material (Material))
import RayTracer.Data.Pattern.Extra (patternAtShape)
import RayTracer.Data.Shape (Shape)
import RayTracer.Data.Tuple (Tuple, normalize, reflect, (.^))

lighting ::
  (Shape o p a, Floating a, RealFrac a) =>
  Material p a ->
  o p a ->
  Light a ->
  Tuple a ->
  Tuple a ->
  Tuple a ->
  Bool ->
  Color a
lighting m object light point eyev normalv inShadow = if inShadow then ambient else ambient + diffuse + specular
  where
    Material pattern_ ambient_ diffuse_ specular_ shininess_ _ = m
    color = fromMaybe black $ patternAtShape pattern_ object point
    effectiveColor = color * intensity light
    lightv = normalize $ position light - point
    ambient = effectiveColor *^ ambient_
    lightDotNormal = lightv .^ normalv
    diffuse
      | lightDotNormal < 0 = black
      | otherwise = effectiveColor *^ diffuse_ *^ lightDotNormal
    reflectv = reflect (- lightv) normalv
    reflectDotEye = reflectv .^ eyev
    factor = reflectDotEye ^ floor shininess_
    specular
      | lightDotNormal < 0 = black
      | reflectDotEye <= 0 = black
      | otherwise = intensity light *^ specular_ *^ factor
