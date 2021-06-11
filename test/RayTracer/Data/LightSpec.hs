module RayTracer.Data.LightSpec
  ( spec,
  )
where

import RayTracer.Data.Color
import RayTracer.Data.Light
import RayTracer.Data.Tuple
import Test.Hspec

spec :: Spec
spec = do
  it "A point light has a position and intensity" $ do
    let intensity_ = color 1 1 1
    let position_ = point 0 0 0
    let light = pointLight position_ intensity_
    position light `shouldBe` position_
    intensity light `shouldBe` intensity_
