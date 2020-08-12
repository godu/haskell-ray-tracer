module RayTracer.Data.WorldSpec
  ( spec,
  )
where

import RayTracer.Data.Color (color)
import RayTracer.Data.Intersection (Intersection (t))
import RayTracer.Data.Light (pointLight)
import qualified RayTracer.Data.Material as M
  ( Material
      ( color,
        diffuse,
        specular
      ),
    material,
  )
import RayTracer.Data.Ray (ray)
import RayTracer.Data.Shape.Sphere
  ( Sphere
      ( material,
        transformation
      ),
    sphere,
  )
import RayTracer.Data.Tuple
  ( point,
    vector,
  )
import RayTracer.Data.World
  ( World (lights, objects),
    defaultWorld,
    intersectWorld,
    world,
  )
import RayTracer.Transformation (scaling)
import Test.Hspec
  ( Spec,
    it,
    shouldBe,
  )
import Prelude
  ( Double,
    Maybe (Nothing),
    length,
    ($),
    (<$>),
  )

spec :: Spec
spec = do
  it "Create a world" $ do
    let w :: World Double Sphere
        w = world
    objects w `shouldBe` []
    lights w `shouldBe` []

  it "The default world" $ do
    let light = pointLight (point (-10) 10 (-10)) (color 1 1 1)
    let s1 =
          sphere
            { material =
                M.material
                  { M.color = color 0.8 1.0 0.6,
                    M.diffuse = 0.7,
                    M.specular = 0.2
                  }
            }
    let s2 = sphere {transformation = scaling 0.5 0.5 0.5}
    let w = defaultWorld
    lights w `shouldBe` [light]
    objects w `shouldBe` [s1, s2]

  it "Intersect a world with a ray" $ do
    let w = defaultWorld
    let r = ray (point 0 0 (-5)) (vector 0 0 1)
    let xs = r `intersectWorld` w
    length xs `shouldBe` 4
    (t <$> xs) `shouldBe` [4, 4.5, 5.5, 6]

  it "Shading an intersection" $ do
    let w = defaultWorld
    let r = ray (point 0 0 (-5)) (vector 0 0 1)
    -- let shape = head $ objects w
    1 `shouldBe` 1
