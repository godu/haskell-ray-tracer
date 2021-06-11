module RayTracer.TransformationSpec
  ( spec,
  )
where

import Data.Maybe (fromJust)
import RayTracer.Data.Matrix
import qualified RayTracer.Data.Tuple as T
import RayTracer.Transformation
import Test.Hspec

spec :: Spec
spec = do
  it "Multiplying by a translation matrix" $ do
    let transform = translation 5 (-3) 2
        p = T.point (-3) 4 5
    transform *^ p `shouldBe` T.point 2 1 7
  it "Multiplying by the inverse of a translation matrix" $ do
    let transform = translation 5 (-3) 2
        inv = fromJust $ inverse transform
        p = T.point (-3) 4 5
    inv *^ p `shouldBe` T.point (-8) 7 3
  it "Translation does not affect vectors" $ do
    let transform = translation 5 (-3) 2
        v = T.vector (-3) 4 5
    transform *^ v `shouldBe` v

  it "A scaling matrix applied to a point" $ do
    let transform = scaling 2 3 4
        p = T.point (-4) 6 8
    transform *^ p `shouldBe` T.point (-8) 18 32
  it "A scaling matrix applied to a vector" $ do
    let transform = scaling 2 3 4
        v = T.vector (-4) 6 8
    transform *^ v `shouldBe` T.vector (-8) 18 32
  it "Multiplying by the inverse of a scaling matrix" $ do
    let transform = scaling 2 3 4
        inv = fromJust $ inverse transform
        v = T.vector (-4) 6 8
    inv *^ v `shouldBe` T.vector (-2) 2 2
  it "Reflexion is scaling by a negative value" $ do
    let transform = scaling (-1) 1 1
        p = T.point 2 3 4
    transform *^ p `shouldBe` T.point (-2) 3 4

  it "Rotating a point around the x axis" $ do
    let p = T.point 0 1 0
        halfQuarter = rotationX (pi / 4)
        fullQuarter = rotationX (pi / 2)
    halfQuarter *^ p `shouldBe` T.point 0 (sqrt 2 / 2) (sqrt 2 / 2)
    fullQuarter *^ p `shouldBe` T.point 0 0 1
  it "The inverse of an x-rotation rotates in the opposite direction" $ do
    let p = T.point 0 1 0
        halfQuarter = rotationX (pi / 4)
        inv = fromJust $ inverse halfQuarter
    inv *^ p `shouldBe` T.point 0 (sqrt 2 / 2) (- sqrt 2 / 2)

  it "Rotating a point around the y axis" $ do
    let p = T.point 0 0 1
        halfQuarter = rotationY (pi / 4)
        fullQuarter = rotationY (pi / 2)
    halfQuarter *^ p `shouldBe` T.point (sqrt 2 / 2) 0 (sqrt 2 / 2)
    fullQuarter *^ p `shouldBe` T.point 1 0 0

  it "Rotating a point around the z axis" $ do
    let p = T.point 0 1 0
        halfQuarter = rotationZ (pi / 4)
        fullQuarter = rotationZ (pi / 2)
    halfQuarter *^ p `shouldBe` T.point (- sqrt 2 / 2) (sqrt 2 / 2) 0
    fullQuarter *^ p `shouldBe` T.point (-1) 0 0

  it "A shearing transformation moves x in propertion to y" $ do
    let transform = shearing 1 0 0 0 0 0
        p = T.point 2 3 4
    transform *^ p `shouldBe` T.point 5 3 4
  it "A shearing transformation moves x in proportion to z" $ do
    let transform = shearing 0 1 0 0 0 0
        p = T.point 2 3 4
    transform *^ p `shouldBe` T.point 6 3 4
  it "A shearing transformation moves y in proportion to x" $ do
    let transform = shearing 0 0 1 0 0 0
        p = T.point 2 3 4
    transform *^ p `shouldBe` T.point 2 5 4
  it "A shearing transformation moves y in proportion to Z" $ do
    let transform = shearing 0 0 0 1 0 0
        p = T.point 2 3 4
    transform *^ p `shouldBe` T.point 2 7 4
  it "A shearing transformation moves y in proportion to Z" $ do
    let transform = shearing 0 0 0 0 1 0
        p = T.point 2 3 4
    transform *^ p `shouldBe` T.point 2 3 6
  it "A shearing transformation moves y in proportion to Z" $ do
    let transform = shearing 0 0 0 0 0 1
        p = T.point 2 3 4
    transform *^ p `shouldBe` T.point 2 3 7
  let p = T.point 1 0 1
      a = rotationX (pi / 2)
      b = scaling 5 5 5
      c = translation 10 5 7
  it "Individual transformations are applied in sequence" $ do
    let p2 = a *^ p
    p2 `shouldBe` T.point 1 (-1) 0
    let p3 = b *^ p2
    p3 `shouldBe` T.point 5 (-5) 0
    let p4 = c *^ p3
    p4 `shouldBe` T.point 15 0 7
  it "Chained transformations must be applied in reverse order" $ do
    let t = c * b * a
    t *^ p `shouldBe` T.point 15 0 7

  it "The transformation matrix for the default orientation" $ do
    let from = T.point 0 0 0
        to = T.point 0 0 (-1)
        up = T.vector 0 1 0
        t = viewTransform from to up
    t `shouldBe` identity
  it "A view transformation matrix looking in positive z direction" $ do
    let from = T.point 0 0 0
        to = T.point 0 0 1
        up = T.vector 0 1 0
        t = viewTransform from to up
    t `shouldBe` scaling (-1) 1 (-1)
  it "The view transformation moves the world" $ do
    let from = T.point 0 0 8
        to = T.point 0 0 0
        up = T.vector 0 1 0
        t = viewTransform from to up
    t `shouldBe` translation 0 0 (-8)
  it "An arbitrary view transformation" $ do
    let from = T.point 1 3 2
        to = T.point 4 (-2) 8
        up = T.vector 1 1 0
        t = viewTransform from to up
    t
      `shouldBe` fromList
        4
        4
        [ -0.50709,
          0.50709,
          0.67612,
          -2.36643,
          0.76772,
          0.60609,
          0.12122,
          -2.82843,
          -0.35857,
          0.59761,
          -0.71714,
          0.00000,
          0.00000,
          0.00000,
          0.00000,
          1.00000
        ]
