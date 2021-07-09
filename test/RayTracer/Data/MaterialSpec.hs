module RayTracer.Data.MaterialSpec
  ( spec,
  )
where

import qualified RayTracer.Data.Color as C
import RayTracer.Data.Light
import RayTracer.Data.Material
import RayTracer.Data.Material.Extra
import qualified RayTracer.Data.Shape.Sphere as S
import RayTracer.Data.Tuple
import qualified RayTracer.Spec as RS
import RayTracer.Transformation
import Test.Hspec

material :: (RealFrac a) => Material RS.Pattern a
material = Material (RS.colorPattern C.white) 0.1 0.9 0.9 200.0

sphere :: (RealFrac a) => S.Sphere RS.Pattern a
sphere = S.Sphere identity material

spec :: Spec
spec = do
  it "The default material" $ do
    let m = material
    pattern_ m `shouldBe` RS.colorPattern (C.color 1 1 1)
    ambient m `shouldBe` 0.1
    diffuse m `shouldBe` 0.9
    specular m `shouldBe` 0.9
    shininess m `shouldBe` 200.0

  let m = material
      position = point 0 0 0
  it "Lighting with the eye between the light and the surface" $ do
    let eyev = vector 0 0 (-1)
        normalv = vector 0 0 (-1)
        light = pointLight (point 0 0 (-10)) (C.color 1 1 1)
    lighting m sphere light position eyev normalv False `shouldBe` C.color 1.9 1.9 1.9
  it "Lighting with the eye between light and surface, eye offset 45°" $ do
    let eyev = vector 0 (sqrt 2 / 2) (- sqrt 2 / 2)
        normalv = vector 0 0 (-1)
        light = pointLight (point 0 0 (-10)) (C.color 1 1 1)
    lighting m sphere light position eyev normalv False `shouldBe` C.color 1.0 1.0 1.0
  it "Lighting with eye opposite surface, light offset 45°" $ do
    let eyev = vector 0 0 (-1)
        normalv = vector 0 0 (-1)
        light = pointLight (point 0 10 (-10)) (C.color 1 1 1)
    lighting m sphere light position eyev normalv False `shouldBe` C.color 0.7364 0.7364 0.7364
  it "Lighting with eye in the path of the reflection vector" $ do
    let eyev = vector 0 (- sqrt 2 / 2) (- sqrt 2 / 2)
        normalv = vector 0 0 (-1)
        light = pointLight (point 0 10 (-10)) (C.color 1 1 1)
    lighting m sphere light position eyev normalv False `shouldBe` C.color 1.6364 1.6364 1.6364
  it "Lighting with the light behind the surface" $ do
    let eyev = vector 0 0 (-1)
        normalv = vector 0 0 (-1)
        light = pointLight (point 0 0 10) (C.color 1 1 1)
    lighting m sphere light position eyev normalv False `shouldBe` C.color 0.1 0.1 0.1

  it "Lighting with the surface in shadow" $ do
    let eyev = vector 0 0 (-1)
        normalv = vector 0 0 (-1)
        light = pointLight (point 0 0 (-10)) (C.color 1 1 1)
        inShadow = True
    lighting m sphere light position eyev normalv inShadow `shouldBe` C.color 0.1 0.1 0.1

  it "Lighting with a pattern applied" $ do
    let m' = m {pattern_ = RS.stripePattern C.white C.black, ambient = 1, diffuse = 0, specular = 0}
        eyev = vector 0 0 (-1)
        normalv = vector 0 0 (-1)
        light = pointLight (point 0 0 (-10)) (C.color 1 1 1)
        inShadow = False
    lighting m' sphere light (point 0.9 0 0) eyev normalv inShadow `shouldBe` C.white
    lighting m' sphere light (point 1.1 0 0) eyev normalv inShadow `shouldBe` C.black
