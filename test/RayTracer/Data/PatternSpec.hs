module RayTracer.Data.PatternSpec
  ( spec,
  )
where

import RayTracer.Data.Pattern
  ( Pattern (a, b),
    black,
    stripeAt,
    stripePattern,
    white,
  )
import RayTracer.Data.Tuple
  ( point,
  )
import Test.Hspec
  ( Spec,
    it,
    shouldBe,
  )
import Prelude
  ( ($),
  )

spec :: Spec
spec = do
  it "Creating a stripe pattern" $ do
    let pattern = stripePattern white black
    a pattern `shouldBe` white
    b pattern `shouldBe` black
  it "A stripe pattern is constant in y" $ do
    let pattern = stripePattern white black
    (pattern `stripeAt` point 0 0 0) `shouldBe` white
    (pattern `stripeAt` point 0 1 0) `shouldBe` white
    (pattern `stripeAt` point 0 2 0) `shouldBe` white
  it "A stripe pattern is constant in z" $ do
    let pattern = stripePattern white black
    pattern `stripeAt` point 0 0 0 `shouldBe` white
    pattern `stripeAt` point 0 0 1 `shouldBe` white
    pattern `stripeAt` point 0 0 2 `shouldBe` white
  it "A stripe pattern alternates in x" $ do
    let pattern = stripePattern white black
    pattern `stripeAt` point 0 0 0 `shouldBe` white
    pattern `stripeAt` point 0.9 0 0 `shouldBe` white
    pattern `stripeAt` point 1 0 0 `shouldBe` black
    pattern `stripeAt` point (-0.1) 0 0 `shouldBe` black
    pattern `stripeAt` point (-1) 0 0 `shouldBe` black
    pattern `stripeAt` point (-1.1) 0 0 `shouldBe` white
