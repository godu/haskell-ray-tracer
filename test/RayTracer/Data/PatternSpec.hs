module RayTracer.Data.PatternSpec
  ( spec,
  )
where

import qualified RayTracer.Data.Pattern as P
  ( Pattern (a, b, transformation),
    black,
    stripePattern,
    white,
  )
import RayTracer.Data.Pattern.Extra
  ( stripeAt,
    stripeAtObject,
  )
import qualified RayTracer.Data.Sphere as S (Sphere (transformation), sphere)
import RayTracer.Data.Tuple
  ( point,
  )
import RayTracer.Transformation (scaling, translation)
import Test.Hspec
  ( Spec,
    it,
    shouldBe,
  )
import Prelude
  ( return,
    ($),
  )

spec :: Spec
spec = do
  it "Creating a stripe pattern" $ do
    let pattern_ = P.stripePattern P.white P.black
    P.a pattern_ `shouldBe` P.white
    P.b pattern_ `shouldBe` P.black
  it "A stripe pattern is constant in y" $ do
    let pattern_ = P.stripePattern P.white P.black
    (pattern_ `stripeAt` point 0 0 0) `shouldBe` P.white
    (pattern_ `stripeAt` point 0 1 0) `shouldBe` P.white
    (pattern_ `stripeAt` point 0 2 0) `shouldBe` P.white
  it "A stripe pattern is constant in z" $ do
    let pattern_ = P.stripePattern P.white P.black
    pattern_ `stripeAt` point 0 0 0 `shouldBe` P.white
    pattern_ `stripeAt` point 0 0 1 `shouldBe` P.white
    pattern_ `stripeAt` point 0 0 2 `shouldBe` P.white
  it "A stripe pattern alternates in x" $ do
    let pattern_ = P.stripePattern P.white P.black
    pattern_ `stripeAt` point 0 0 0 `shouldBe` P.white
    pattern_ `stripeAt` point 0.9 0 0 `shouldBe` P.white
    pattern_ `stripeAt` point 1 0 0 `shouldBe` P.black
    pattern_ `stripeAt` point (-0.1) 0 0 `shouldBe` P.black
    pattern_ `stripeAt` point (-1) 0 0 `shouldBe` P.black
    pattern_ `stripeAt` point (-1.1) 0 0 `shouldBe` P.white

  it "Stripes with an object transformation" $ do
    let object = S.sphere {S.transformation = scaling 2 2 2}
        pattern_ = P.stripePattern P.white P.black
    stripeAtObject pattern_ object (point 1.5 0 0) `shouldBe` return P.white
  it "Stripes with a pattern transformation" $ do
    let object = S.sphere {S.transformation = scaling 2 2 2}
        pattern_ = (P.stripePattern P.white P.black) {P.transformation = scaling 2 2 2}
    stripeAtObject pattern_ object (point 1.5 0 0) `shouldBe` return P.white
  it "Stripes with both an object and a pattern transformation" $ do
    let object = S.sphere {S.transformation = scaling 2 2 2}
        pattern_ = (P.stripePattern P.white P.black) {P.transformation = translation 0.5 0 0}
    stripeAtObject pattern_ object (point 2.5 0 0) `shouldBe` return P.white
