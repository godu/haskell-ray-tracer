module RayTracer.Data.MaterialSpec
  ( spec,
  )
where

import qualified RayTracer.Data.Color as C
  ( color,
  )
import RayTracer.Data.Light (pointLight)
import RayTracer.Data.Material
  ( Material
      ( ambient,
        color,
        diffuse,
        shininess,
        specular
      ),
    lighting,
    material,
  )
import RayTracer.Data.Tuple
  ( point,
    vector,
  )
import Test.Hspec
  ( Spec,
    it,
    shouldBe,
  )
import Prelude
  ( sqrt,
    ($),
    (/),
  )

spec :: Spec
spec = do
  it "The default material" $ do
    let m = material
    color m `shouldBe` C.color 1 1 1
    ambient m `shouldBe` 0.1
    diffuse m `shouldBe` 0.9
    specular m `shouldBe` 0.9
    shininess m `shouldBe` 200.0

  let m = material
  let position = point 0 0 0
  it "Lighting with the eye between the light and the surface" $ do
    let eyev = vector 0 0 (-1)
    let normalv = vector 0 0 (-1)
    let light = pointLight (point 0 0 (-10)) (C.color 1 1 1)
    lighting m light position eyev normalv `shouldBe` C.color 1.9 1.9 1.9
  it "Lighting with the eye between light and surface, eye offset 45°" $ do
    let eyev = vector 0 (sqrt 2 / 2) (- sqrt 2 / 2)
    let normalv = vector 0 0 (-1)
    let light = pointLight (point 0 0 (-10)) (C.color 1 1 1)
    lighting m light position eyev normalv `shouldBe` C.color 1.0 1.0 1.0
  it "Lighting with eye opposite surface, light offset 45°" $ do
    let eyev = vector 0 0 (-1)
    let normalv = vector 0 0 (-1)
    let light = pointLight (point 0 10 (-10)) (C.color 1 1 1)
    lighting m light position eyev normalv
      `shouldBe` C.color 0.7364 0.7364 0.7364
  it "Lighting with eye in the path of the reflection vector" $ do
    let eyev = vector 0 (- sqrt 2 / 2) (- sqrt 2 / 2)
    let normalv = vector 0 0 (-1)
    let light = pointLight (point 0 10 (-10)) (C.color 1 1 1)
    lighting m light position eyev normalv
      `shouldBe` C.color 1.6364 1.6364 1.6364
  it "Lighting with the light behind the surface" $ do
    let eyev = vector 0 0 (-1)
    let normalv = vector 0 0 (-1)
    let light = pointLight (point 0 0 10) (C.color 1 1 1)
    lighting m light position eyev normalv `shouldBe` C.color 0.1 0.1 0.1
