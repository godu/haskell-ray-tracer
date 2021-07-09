module RayTracer.Data.Material.Extra
  ( lighting,
  )
where

import Data.Maybe
import RayTracer.Data.Color
import qualified RayTracer.Data.Color as C
import RayTracer.Data.Light
import RayTracer.Data.Material
import RayTracer.Data.Pattern.Extra
import qualified RayTracer.Data.Shape as S
import RayTracer.Data.Tuple

lighting ::
  (S.Shape o p a, Floating a, RealFrac a) =>
  Material p a ->
  o p a ->
  Light a ->
  Tuple a ->
  Tuple a ->
  Tuple a ->
  Bool ->
  C.Color a
lighting m object light point eyev normalv inShadow = if inShadow then ambient else ambient + diffuse + specular
  where
    Material pattern_ ambient_ diffuse_ specular_ shininess_ = m
    color = fromMaybe black $ patternAtShape pattern_ object point
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
