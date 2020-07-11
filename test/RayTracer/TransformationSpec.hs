module RayTracer.TransformationSpec
  ( spec
  )
where

import           Prelude                        ( ($)
                                                , (*)
                                                , (/)
                                                , pi
                                                , sqrt
                                                )
import           Data.Maybe                     ( fromJust )
import           RayTracer.Transformation       ( translation
                                                , scaling
                                                , rotationX
                                                , rotationY
                                                , rotationZ
                                                )
import           RayTracer.Data.Tuple           ( point
                                                , vector
                                                )
import           RayTracer.Data.Matrix          ( (*^)
                                                , inverse
                                                )
import           Test.Hspec                     ( Spec
                                                , it
                                                , shouldBe
                                                )

spec :: Spec
spec = do
  it "Multiplying by a translation matrix" $ do
    let transform = translation 5 (-3) 2
    let p         = point (-3) 4 5
    transform *^ p `shouldBe` point 2 1 7
  it "Multiplying by the inverse of a translation matrix" $ do
    let transform = translation 5 (-3) 2
    let inv       = fromJust $ inverse transform
    let p         = point (-3) 4 5
    inv *^ p `shouldBe` point (-8) 7 3
  it "Translation does not affect vectors" $ do
    let transform = translation 5 (-3) 2
    let v         = vector (-3) 4 5
    transform *^ v `shouldBe` v

  it "A scaling matrix applied to a point" $ do
    let transform = scaling 2 3 4
    let p         = point (-4) 6 8
    transform *^ p `shouldBe` point (-8) 18 32
  it "A scaling matrix applied to a vector" $ do
    let transform = scaling 2 3 4
    let v         = vector (-4) 6 8
    transform *^ v `shouldBe` vector (-8) 18 32
  it "Multiplying by the inverse of a scaling matrix" $ do
    let transform = scaling 2 3 4
    let inv       = fromJust $ inverse transform
    let v         = vector (-4) 6 8
    inv *^ v `shouldBe` vector (-2) 2 2
  it "Reflexion is scaling by a negative value" $ do
    let transform = scaling (-1) 1 1
    let p         = point 2 3 4
    transform *^ p `shouldBe` point (-2) 3 4

  it "Rotating a point around the x axis" $ do
    let p           = point 0 1 0
    let halfQuarter = rotationX (pi / 4)
    let fullQuarter = rotationX (pi / 2)
    halfQuarter *^ p `shouldBe` point 0 (sqrt 2 / 2) (sqrt 2 / 2)
    fullQuarter *^ p `shouldBe` point 0 0 1
  it "The inverse of an x-rotation rotates in the opposite direction" $ do
    let p           = point 0 1 0
    let halfQuarter = rotationX (pi / 4)
    let inv         = fromJust $ inverse halfQuarter
    inv *^ p `shouldBe` point 0 (sqrt 2 / 2) (-(sqrt 2) / 2)

  it "Rotating a point around the y axis" $ do
    let p           = point 0 0 1
    let halfQuarter = rotationY (pi / 4)
    let fullQuarter = rotationY (pi / 2)
    halfQuarter *^ p `shouldBe` point (sqrt 2 / 2) 0 (sqrt 2 / 2)
    fullQuarter *^ p `shouldBe` point 1 0 0

  it "Rotating a point around the z axis" $ do
    let p           = point 0 1 0
    let halfQuarter = rotationZ (pi / 4)
    let fullQuarter = rotationZ (pi / 2)
    halfQuarter *^ p `shouldBe` point (-(sqrt 2) / 2) (sqrt 2 / 2) 0
    fullQuarter *^ p `shouldBe` point (-1) 0 0

