module RayTracer.Data.WorldSpec
  ( spec,
  )
where

import RayTracer.Data.Color (color)
import RayTracer.Data.Intersection (intersection, t)
import RayTracer.Data.Intersection.Computations (prepareComputations)
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
import RayTracer.Data.World (World (lights, objects), defaultWorld, intersect, shadeHit, world)
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
    head,
    return,
    (!!),
    ($),
  )

spec :: Spec
spec = do
  it "Creating a world" $ do
    let actual = world
    objects actual `shouldBe` []
    lights actual `shouldBe` []

  it "The default world" $ do
    let l = pointLight (point (-10) 10 (-10)) (color 1 1 1)
        s1 =
          sphere
            { material =
                M.material
                  { M.color = color 0.8 1.0 0.6,
                    M.diffuse = 0.7,
                    M.specular = 0.2
                  }
            }
        s2 =
          sphere
            { transformation = scaling 0.5 0.5 0.5
            }
        w = defaultWorld
    lights w `shouldBe` return l
    objects w `shouldContain` [s1]
    objects w `shouldContain` [s2]

  it "Intersect a world with a ray" $ do
    let w = defaultWorld
        r = ray (point 0 0 (-5)) (vector 0 0 1)
        xs = r `intersect` w
    fmap t xs `shouldBe` [4, 4.5, 5.5, 6]

  it "Shading an intersection" $ do
    let w = defaultWorld
        r = ray (point 0 0 (-5)) (vector 0 0 1)
        shape = head $ objects w
        i = intersection 4 shape
        comps = prepareComputations i r
        c = shadeHit w comps
    c `shouldBe` color 0.38066 0.47583 0.2855
  it "Shading an intersection from the inside" $ do
    let w =
          defaultWorld
            { lights = return $ pointLight (point 0 0.25 0) (color 1 1 1)
            }
        r = ray (point 0 0 0) (vector 0 0 1)
        shape = objects w !! 1
        i = intersection 0.5 shape
        comps = prepareComputations i r
        c = shadeHit w comps
    c `shouldBe` color 0.90498 0.90498 0.90498