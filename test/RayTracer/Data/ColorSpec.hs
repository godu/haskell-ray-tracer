module RayTracer.Data.ColorSpec
  ( spec,
  )
where

import RayTracer.Data.Color
import Test.Hspec

spec :: Spec
spec = do
  it "Colors are (red, green, blue) tuples" $ do
    let actual = color (-0.5) 0.4 1.7
    red actual `shouldBe` -0.5
    green actual `shouldBe` 0.4
    blue actual `shouldBe` 1.7

  it "Adding colors" $ do
    let a = color 0.9 0.6 0.75
    let b = color 0.7 0.1 0.25
    a + b `shouldBe` color 1.6 0.7 1.0
  it "Substracting colors" $ do
    let a = color 0.9 0.6 0.75
    let b = color 0.7 0.1 0.25
    a - b `shouldBe` color 0.2 0.5 0.5
  it "Multipling a color by a scalar" $ do
    let actual = color 0.2 0.3 0.4
    actual *^ 2 `shouldBe` color 0.4 0.6 0.8
  it "Multipling colors" $ do
    let a = color 1 0.2 0.4
    let b = color 0.9 1 0.1
    a * b `shouldBe` color 0.9 0.2 0.04
