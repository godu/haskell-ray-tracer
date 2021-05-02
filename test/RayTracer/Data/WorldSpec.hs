module RayTracer.Data.WorldSpec
  ( spec,
  )
where

import RayTracer.Data.Color (color)
import RayTracer.Data.Intersection (t)
import RayTracer.Data.Light (pointLight)
import qualified RayTracer.Data.Material as M
  ( Material (ambient, specular),
    color,
    diffuse,
    material,
  )
import RayTracer.Data.Ray (ray)
import RayTracer.Data.Sphere (material, sphere, transformation)
import RayTracer.Data.Tuple (point, vector)
import RayTracer.Data.World (World (light, objects), defaultWorld, intersect, world)
import RayTracer.Transformation (scaling)
import Test.Hspec
  ( Spec,
    it,
    shouldBe,
    shouldContain,
  )
import Prelude
  ( Maybe (Nothing),
    fmap,
    return,
    ($),
  )

spec :: Spec
spec = do
  it "Creating a world" $ do
    let actual = world
    objects actual `shouldBe` []
    light actual `shouldBe` Nothing

  it "The default world" $ do
    let l = pointLight (point (-10) 10 (-10)) (color 1 1 1)
    let s1 =
          sphere
            { material =
                M.material
                  { M.color = color 0.8 1.0 0.6,
                    M.diffuse = 0.7,
                    M.specular = 0.2
                  }
            }
    let s2 =
          sphere
            { transformation = scaling 0.5 0.5 0.5
            }
    let w = defaultWorld
    light w `shouldBe` return l
    objects w `shouldContain` [s1]
    objects w `shouldContain` [s2]

  it "Intersect a world with a ray" $ do
    let w = defaultWorld
    let r = ray (point 0 0 (-5)) (vector 0 0 1)
    let xs = r `intersect` w
    fmap t xs `shouldBe` [4, 4.5, 5.5, 6]