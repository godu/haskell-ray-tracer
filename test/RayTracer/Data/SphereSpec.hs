module RayTracer.Data.SphereSpec
  ( spec
  )
where

import           Prelude                        ( ($)
                                                , length
                                                )
import           RayTracer.Data.Tuple           ( point
                                                , vector
                                                )
import           RayTracer.Data.Ray             ( ray )
import           RayTracer.Data.Sphere          ( sphere
                                                , intersect
                                                )
import           Test.Hspec                     ( Spec
                                                , it
                                                , shouldBe
                                                )


spec :: Spec
spec = do
  it "A ray intersects a sphere at two points" $ do
    let r  = ray (point 0 0 (-5)) (vector 0 0 1)
    let s  = sphere
    let xs = r `intersect` s
    length xs `shouldBe` 2
    xs `shouldBe` [4.0, 6.0]
  it "A ray intersects a sphere at a tangent" $ do
    let r  = ray (point 0 1 (-5)) (vector 0 0 1)
    let s  = sphere
    let xs = r `intersect` s
    length xs `shouldBe` 2
    xs `shouldBe` [5.0, 5.0]
  it "A ray misses a sphere" $ do
    let r  = ray (point 0 2 (-5)) (vector 0 0 1)
    let s  = sphere
    let xs = r `intersect` s
    length xs `shouldBe` 0
  it "A ray originates inside a sphere" $ do
    let r  = ray (point 0 0 0) (vector 0 0 1)
    let s  = sphere
    let xs = r `intersect` s
    length xs `shouldBe` 2
    xs `shouldBe` [-1.0, 1.0]
  it "A sphere is behind a ray" $ do
    let r  = ray (point 0 0 5) (vector 0 0 1)
    let s  = sphere
    let xs = r `intersect` s
    length xs `shouldBe` 2
    xs `shouldBe` [-6.0, -4.0]
