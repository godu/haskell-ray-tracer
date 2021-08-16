module RayTracer.Data.PatternSpec
  ( spec,
  )
where

import RayTracer.Data.Color
import qualified RayTracer.Data.Material as M
import qualified RayTracer.Data.Matrix as M
import qualified RayTracer.Data.Pattern as P
import qualified RayTracer.Data.Pattern.CheckersPattern as CP
import RayTracer.Data.Pattern.ColorPattern (ColorPattern, colorPattern)
import RayTracer.Data.Pattern.Extra
import qualified RayTracer.Data.Pattern.GradientPattern as GP
import qualified RayTracer.Data.Pattern.RingPattern as RP
import qualified RayTracer.Data.Pattern.StripePattern as SP
import qualified RayTracer.Data.Shape.Sphere as S
import RayTracer.Data.Tuple
import RayTracer.Transformation
import Test.Hspec

material :: M.Material (SP.StripePattern ColorPattern) Double
material = M.Material pattern_ 0.1 0.9 0.9 200.0 0.0
  where
    pattern_ = SP.stripePattern whitePattern blackPattern

sphere :: S.Sphere (SP.StripePattern ColorPattern) Double
sphere = S.Sphere identity material

newtype TestPattern a = TestPattern
  { transformation :: M.Matrix a
  }
  deriving (Show, Eq)

instance (Ord a, Fractional a) => P.Pattern TestPattern a where
  getTransformation = transformation
  setTransformation p t = p {transformation = t}
  patternAt _ p = pure $ color (x p) (y p) (z p)

testPattern :: TestPattern Double
testPattern = TestPattern identity

testMaterial :: M.Material TestPattern Double
testMaterial = M.Material testPattern 0.1 0.9 0.9 200.0 0.0

testSphere :: S.Sphere TestPattern Double
testSphere = S.Sphere identity testMaterial

whitePattern :: ColorPattern Double
whitePattern = colorPattern white

blackPattern :: ColorPattern Double
blackPattern = colorPattern black

spec :: Spec
spec = do
  it "Creating a stripe pattern" $ do
    let pattern_ = SP.stripePattern whitePattern blackPattern
    SP.a pattern_ `shouldBe` whitePattern
    SP.b pattern_ `shouldBe` blackPattern
  it "A stripe pattern is constant in y" $ do
    let pattern_ = SP.stripePattern whitePattern blackPattern
    (pattern_ `P.patternAt` point 0 0 0) `shouldBe` pure white
    (pattern_ `P.patternAt` point 0 1 0) `shouldBe` pure white
    (pattern_ `P.patternAt` point 0 2 0) `shouldBe` pure white
  it "A stripe pattern is constant in z" $ do
    let pattern_ = SP.stripePattern whitePattern blackPattern
    pattern_ `P.patternAt` point 0 0 0 `shouldBe` pure white
    pattern_ `P.patternAt` point 0 0 1 `shouldBe` pure white
    pattern_ `P.patternAt` point 0 0 2 `shouldBe` pure white
  it "A stripe pattern alternates in x" $ do
    let pattern_ = SP.stripePattern whitePattern blackPattern
    pattern_ `P.patternAt` point 0 0 0 `shouldBe` pure white
    pattern_ `P.patternAt` point 0.9 0 0 `shouldBe` pure white
    pattern_ `P.patternAt` point 1 0 0 `shouldBe` pure black
    pattern_ `P.patternAt` point (-0.1) 0 0 `shouldBe` pure black
    pattern_ `P.patternAt` point (-1) 0 0 `shouldBe` pure black
    pattern_ `P.patternAt` point (-1.1) 0 0 `shouldBe` pure white

  it "Stripes with an object transformation" $ do
    let object = sphere {S.transformation = scaling 2 2 2}
        pattern_ = SP.stripePattern whitePattern blackPattern
    patternAtShape pattern_ object (point 1.5 0 0) `shouldBe` pure white
  it "Stripes with a pattern transformation" $ do
    let object = sphere {S.transformation = scaling 2 2 2}
        pattern_ = P.setTransformation (SP.stripePattern whitePattern blackPattern) (scaling 2 2 2)
    patternAtShape pattern_ object (point 1.5 0 0) `shouldBe` pure white
  it "Stripes with both an object and a pattern transformation" $ do
    let object = sphere {S.transformation = scaling 2 2 2}
        pattern_ = P.setTransformation (SP.stripePattern whitePattern blackPattern) (translation 0.5 0 0)
    patternAtShape pattern_ object (point 2.5 0 0) `shouldBe` pure white

  it "The default pattern transformation" $ do
    let pattern_ = testPattern
    P.getTransformation pattern_ `shouldBe` identity
  it "Assigning a transformation" $ do
    let pattern_ = testPattern {transformation = translation 1 2 3}
    P.getTransformation pattern_ `shouldBe` translation 1 2 3

  it "A pattern with an object transformation" $ do
    let shape = testSphere {S.transformation = scaling 2 2 2}
        pattern_ = testPattern
    patternAtShape pattern_ shape (point 2 3 4) `shouldBe` pure (color 1 1.5 2)
  it "A pattern with a pattern transformation" $ do
    let shape = testSphere
        pattern_ = testPattern {transformation = scaling 2 2 2}
    patternAtShape pattern_ shape (point 2 3 4) `shouldBe` pure (color 1 1.5 2)
  it "A pattern with both an object and a pattern transformation" $ do
    let shape = testSphere {S.transformation = scaling 2 2 2}
        pattern_ = testPattern {transformation = translation 0.5 1 1.5}
    patternAtShape pattern_ shape (point 2.5 3 3.5) `shouldBe` pure (color 0.75 0.5 0.25)

  it "A gradient linearly interpolates between colors" $ do
    let pattern_ = GP.gradientPattern whitePattern blackPattern
    pattern_ `P.patternAt` point 0 0 0 `shouldBe` pure white
    pattern_ `P.patternAt` point 0.25 0 0 `shouldBe` pure (color 0.75 0.75 0.75)
    pattern_ `P.patternAt` point 0.5 0 0 `shouldBe` pure (color 0.5 0.5 0.5)
    pattern_ `P.patternAt` point 0.75 0 0 `shouldBe` pure (color 0.25 0.25 0.25)

  it "A ring should extend in both x and z" $ do
    let pattern_ = RP.ringPattern whitePattern blackPattern
    pattern_ `P.patternAt` point 0 0 0 `shouldBe` pure white
    pattern_ `P.patternAt` point 1 0 0 `shouldBe` pure black
    pattern_ `P.patternAt` point 0 0 1 `shouldBe` pure black
    pattern_ `P.patternAt` point 0.708 0 0.708 `shouldBe` pure black

  it "Checkers should repeat in x" $ do
    let pattern_ = CP.checkersPattern whitePattern blackPattern
    pattern_ `P.patternAt` point 0 0 0 `shouldBe` pure white
    pattern_ `P.patternAt` point 0.99 0 0 `shouldBe` pure white
    pattern_ `P.patternAt` point 1.01 0 0 `shouldBe` pure black
  it "Checkers should repeat in y" $ do
    let pattern_ = CP.checkersPattern whitePattern blackPattern
    pattern_ `P.patternAt` point 0 0 0 `shouldBe` pure white
    pattern_ `P.patternAt` point 0 0.99 0 `shouldBe` pure white
    pattern_ `P.patternAt` point 0 1.01 0 `shouldBe` pure black
  it "Checkers should repeat in z" $ do
    let pattern_ = CP.checkersPattern whitePattern blackPattern
    pattern_ `P.patternAt` point 0 0 0 `shouldBe` pure white
    pattern_ `P.patternAt` point 0 0 0.99 `shouldBe` pure white
    pattern_ `P.patternAt` point 0 0 1.01 `shouldBe` pure black
