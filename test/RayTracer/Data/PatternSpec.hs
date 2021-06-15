{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module RayTracer.Data.PatternSpec
  ( spec,
  )
where

import RayTracer.Data.Color
import qualified RayTracer.Data.Material as M
import qualified RayTracer.Data.Matrix as M
import qualified RayTracer.Data.Pattern as P
import RayTracer.Data.Pattern.Extra
import qualified RayTracer.Data.Pattern.GradientPattern as GP
import qualified RayTracer.Data.Pattern.StripePattern as SP
import qualified RayTracer.Data.Shape.Sphere as S
import RayTracer.Data.Tuple
import RayTracer.Transformation
import Test.Hspec

material :: (RealFrac a) => M.Material SP.StripePattern a
material = M.Material (SP.stripePattern white black) 0.1 0.9 0.9 200.0

sphere :: (Num a, RealFrac a) => S.Sphere SP.StripePattern a
sphere = S.Sphere identity material

newtype TestPattern a = TestPattern
  { transformation :: M.Matrix a
  }
  deriving (Show, Eq)

instance (Ord a, Fractional a) => P.Pattern TestPattern a where
  transformation = transformation
  patternAt _ p = color (x p) (y p) (z p)

testPattern :: TestPattern Double
testPattern = TestPattern identity

testMaterial :: M.Material TestPattern Double
testMaterial = M.Material testPattern 0.1 0.9 0.9 200.0

testSphere :: S.Sphere TestPattern Double
testSphere = S.Sphere identity testMaterial

spec :: Spec
spec = do
  it "Creating a stripe pattern" $ do
    let pattern_ = SP.stripePattern white black
    SP.a pattern_ `shouldBe` white
    SP.b pattern_ `shouldBe` black
  it "A stripe pattern is constant in y" $ do
    let pattern_ = SP.stripePattern white black
    (pattern_ `P.patternAt` point 0 0 0) `shouldBe` white
    (pattern_ `P.patternAt` point 0 1 0) `shouldBe` white
    (pattern_ `P.patternAt` point 0 2 0) `shouldBe` white
  it "A stripe pattern is constant in z" $ do
    let pattern_ = SP.stripePattern white black
    pattern_ `P.patternAt` point 0 0 0 `shouldBe` white
    pattern_ `P.patternAt` point 0 0 1 `shouldBe` white
    pattern_ `P.patternAt` point 0 0 2 `shouldBe` white
  it "A stripe pattern alternates in x" $ do
    let pattern_ = SP.stripePattern white black
    pattern_ `P.patternAt` point 0 0 0 `shouldBe` white
    pattern_ `P.patternAt` point 0.9 0 0 `shouldBe` white
    pattern_ `P.patternAt` point 1 0 0 `shouldBe` black
    pattern_ `P.patternAt` point (-0.1) 0 0 `shouldBe` black
    pattern_ `P.patternAt` point (-1) 0 0 `shouldBe` black
    pattern_ `P.patternAt` point (-1.1) 0 0 `shouldBe` white

  it "Stripes with an object transformation" $ do
    let object = sphere {S.transformation = scaling 2 2 2}
        pattern_ = SP.stripePattern white black
    patternAtShape pattern_ object (point 1.5 0 0) `shouldBe` return white
  it "Stripes with a pattern transformation" $ do
    let object = sphere {S.transformation = scaling 2 2 2}
        pattern_ = (SP.stripePattern white black) {SP.transformation = scaling 2 2 2}
    patternAtShape pattern_ object (point 1.5 0 0) `shouldBe` return white
  it "Stripes with both an object and a pattern transformation" $ do
    let object = sphere {S.transformation = scaling 2 2 2}
        pattern_ = (SP.stripePattern white black) {SP.transformation = translation 0.5 0 0}
    patternAtShape pattern_ object (point 2.5 0 0) `shouldBe` return white

  it "The default pattern transformation" $ do
    let pattern_ = testPattern
    P.transformation pattern_ `shouldBe` identity
  it "Assigning a transformation" $ do
    let pattern_ = testPattern {transformation = translation 1 2 3}
    P.transformation pattern_ `shouldBe` translation 1 2 3

  it "A pattern with an object transformation" $ do
    let shape = testSphere {S.transformation = scaling 2 2 2}
        pattern_ = testPattern
    patternAtShape pattern_ shape (point 2 3 4) `shouldBe` return (color 1 1.5 2)
  it "A pattern with a pattern transformation" $ do
    let shape = testSphere
        pattern_ = testPattern {transformation = scaling 2 2 2}
    patternAtShape pattern_ shape (point 2 3 4) `shouldBe` return (color 1 1.5 2)
  it "A pattern with both an object and a pattern transformation" $ do
    let shape = testSphere {S.transformation = scaling 2 2 2}
        pattern_ = testPattern {transformation = translation 0.5 1 1.5}
    patternAtShape pattern_ shape (point 2.5 3 3.5) `shouldBe` return (color 0.75 0.5 0.25)

  it "A gradient liearly interpolates between colors" $ do
    let pattern_ = GP.gradientPattern white black
    pattern_ `P.patternAt` point 0 0 0 `shouldBe` white
    pattern_ `P.patternAt` point 0.25 0 0 `shouldBe` color 0.75 0.75 0.75
    pattern_ `P.patternAt` point 0.5 0 0 `shouldBe` color 0.5 0.5 0.5
    pattern_ `P.patternAt` point 0.75 0 0 `shouldBe` color 0.25 0.25 0.25
