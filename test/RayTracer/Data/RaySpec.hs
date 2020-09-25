module RayTracer.Data.RaySpec
  ( spec,
  )
where

import RayTracer.Data.Ray
  ( direction,
    origin,
    position,
    ray,
    transform,
  )
import RayTracer.Data.Tuple
  ( point,
    vector,
  )
import RayTracer.Transformation
  ( scaling,
    translation,
  )
import Test.Hspec
  ( Spec,
    it,
    shouldBe,
  )
import Prelude (($))

spec :: Spec
spec = do
  it "Creating and querying a ray" $ do
    let o = point 1 2 3
    let d = vector 4 5 6
    let r = ray o d
    origin r `shouldBe` o
    direction r `shouldBe` d
  it "Computing a point from a distance" $ do
    let r = ray (point 2 3 4) (vector 1 0 0)
    position 0 r `shouldBe` point 2 3 4
    position 1 r `shouldBe` point 3 3 4
    position (-1) r `shouldBe` point 1 3 4
    position 2.5 r `shouldBe` point 4.5 3 4

  it "Translating a ray" $ do
    let r = ray (point 1 2 3) (vector 0 1 0)
    let m = translation 3 4 5
    let r2 = m `transform` r
    origin r2 `shouldBe` point 4 6 8
    direction r2 `shouldBe` vector 0 1 0
  it "Scaling a ray" $ do
    let r = ray (point 1 2 3) (vector 0 1 0)
    let m = scaling 2 3 4
    let r2 = m `transform` r
    origin r2 `shouldBe` point 2 6 12
    direction r2 `shouldBe` vector 0 3 0
