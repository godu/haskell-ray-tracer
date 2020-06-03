module RayTracer.Data.TupleSpec where

import           Test.Hspec
import           RayTracer.Data.Tuple

spec :: Spec
spec = do
  it "A tuple with w=1.0 is a point" $ do
    let actual = tuple 4.3 (-4.2) 3.1 1.0
    x actual `shouldBe` 4.3
    y actual `shouldBe` (-4.2)
    z actual `shouldBe` 3.1
    w actual `shouldBe` 1.0
    isPoint actual `shouldBe` True
    isVector actual `shouldBe` False
  it "A tuple with w=0 is a vector" $ do
    let actual = tuple 4.3 (-4.2) 3.1 0.0
    x actual `shouldBe` 4.3
    y actual `shouldBe` (-4.2)
    z actual `shouldBe` 3.1
    w actual `shouldBe` 0.0
    isPoint actual `shouldBe` False
    isVector actual `shouldBe` True

  it "point() creates tuples with w=1" $ do
    let actual = point 4 (-4) 3
    actual `shouldBe` tuple 4 (-4) 3 1
  it "vector() creates tuples with w=1" $ do
    let actual = vector 4 (-4) 3
    actual `shouldBe` tuple 4 (-4) 3 0

  it "Subtracting two points" $ do
    let a = point 3 2 1
        b = point 5 6 7
    (a - b) `shouldBe` vector (-2) (-4) (-6)
  it "Subtracting a vector from a point" $ do
    let a = point 3 2 1
        b = vector 5 6 7
    (a - b) `shouldBe` point (-2) (-4) (-6)
  it "Subtracting two vectors" $ do
    let a = vector 3 2 1
        b = vector 5 6 7
    (a - b) `shouldBe` vector (-2) (-4) (-6)

  it "Subtracting a vector from the zero vector" $ do
    let actual = vector 1 (-2) 3
    zero - actual `shouldBe` vector (-1) 2 (-3)

  it "Negating a tuple" $ do
    let actual = tuple 1 (-2) 3 (-4)
    (-actual) `shouldBe` tuple (-1) 2 (-3) 4

  it "Multiplying a tuple by a scalar" $ do
    let actual = tuple 1 (-2) 3 (-4)
    actual *^ 3.5 `shouldBe` tuple 3.5 (-7) 10.5 (-14)
  it "Multiplying a tuple by a fraction" $ do
    let actual = tuple 1 (-2) 3 (-4)
    actual *^ 0.5 `shouldBe` tuple 0.5 (-1) 1.5 (-2)

  it "Dividing a tuple by a fraction" $ do
    let actual = tuple 1 (-2) 3 (-4)
    actual /^ 2 `shouldBe` tuple 0.5 (-1) 1.5 (-2)

  it "Computing the magnitude of vector(1, 0, 0)" $ do
    let actual = vector 1 0 0
    magnitude actual `shouldBe` 1
  it "Computing the magnitude of vector(0, 1, 0)" $ do
    let actual = vector 0 1 0
    magnitude actual `shouldBe` 1
  it "Computing the magnitude of vector(0, 0, 1)" $ do
    let actual = vector 0 0 1
    magnitude actual `shouldBe` 1
  it "Computing the magnitude of vector(1, 2, 3)" $ do
    let actual = vector 1 2 3
    magnitude actual `shouldBe` sqrt 14
  it "Computing the magnitude of vector(0, 0, 1)" $ do
    let actual = vector (-1) (-2) (-3)
    magnitude actual `shouldBe` sqrt 14
