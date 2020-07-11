module RayTracer.Data.MatrixSpec
  ( spec
  )
where

import           Prelude                        ( ($)
                                                , (<$>)
                                                , (=<<)
                                                , (*)
                                                , (/)
                                                , pure
                                                , map
                                                )
import           Data.Maybe                     ( fromJust )
import           Test.Hspec                     ( Spec
                                                , it
                                                , shouldBe
                                                , shouldNotBe
                                                )
import           RayTracer.Data.Tuple           ( tuple )
import           RayTracer.Data.Matrix          ( (*^)
                                                , fromList
                                                , at
                                                , one
                                                , transpose
                                                , determinant
                                                , submatrix
                                                , minor
                                                , cofactor
                                                , inverse
                                                )

spec :: Spec
spec = do
  it "Constructing and inspecting a 4x4 matrix" $ do
    let m = fromList
          4
          4
          [ 1
          , 2
          , 3
          , 4
          , 5.5
          , 6.5
          , 7.5
          , 8.5
          , 9
          , 10
          , 11
          , 12
          , 13.5
          , 14.5
          , 15.5
          , 16.5
          ]
    m `at` (0, 0) `shouldBe` pure 1
    m `at` (0, 3) `shouldBe` pure 4
    m `at` (1, 0) `shouldBe` pure 5.5
    m `at` (1, 2) `shouldBe` pure 7.5
    m `at` (2, 2) `shouldBe` pure 11
    m `at` (3, 0) `shouldBe` pure 13.5
    m `at` (3, 2) `shouldBe` pure 15.5
  it "A 2x2 matrix ought to be representable" $ do
    let m = fromList 2 2 [-3, 5, 1, -2]
    m `at` (0, 0) `shouldBe` pure (-3)
    m `at` (0, 1) `shouldBe` pure 5
    m `at` (1, 0) `shouldBe` pure 1
    m `at` (1, 1) `shouldBe` pure (-2)
  it "A 3x3 matrix ought to be representable" $ do
    let m = fromList 3 3 [-3, 5, 0, 1, -2, -7, 0, 1, 1]
    m `at` (0, 0) `shouldBe` pure (-3)
    m `at` (1, 1) `shouldBe` pure (-2)
    m `at` (2, 2) `shouldBe` pure 1

  it "Matrix equality with identical matrices" $ do
    let a = fromList 4 4 [1, 2, 3, 4, 5, 6, 7, 8, 9, 8, 7, 6, 5, 4, 3, 2]
    let b = fromList 4 4 [1, 2, 3, 4, 5, 6, 7, 8, 9, 8, 7, 6, 5, 4, 3, 2]
    a `shouldBe` b
    a `shouldBe` b
    a `shouldBe` b
  it "Matrix equality with different matrices" $ do
    let a = fromList 4 4 [1, 2, 3, 4, 5, 6, 7, 8, 9, 8, 7, 6, 5, 4, 3, 2]
    let b = fromList 4 4 [2, 3, 4, 5, 6, 7, 8, 9, 8, 7, 6, 5, 4, 3, 2, 1]
    a `shouldNotBe` b
    a `shouldNotBe` b
    a `shouldNotBe` b

  it "Multiplying two matrices" $ do
    let a = fromList 4 4 [1, 2, 3, 4, 5, 6, 7, 8, 9, 8, 7, 6, 5, 4, 3, 2]
    let b = fromList 4 4 [-2, 1, 2, 3, 3, 2, 1, -1, 4, 3, 6, 5, 1, 2, 7, 8]
    a * b `shouldBe` fromList
      4
      4
      [20, 22, 50, 48, 44, 54, 114, 108, 40, 58, 110, 102, 16, 26, 46, 42]

  it "A matrix multiplied by a tuple" $ do
    let m = fromList 4 4 [1, 2, 3, 4, 2, 4, 4, 2, 8, 6, 4, 1, 0, 0, 0, 1]
    let t = tuple 1 2 3 1
    m *^ t `shouldBe` tuple 18 24 33 1

  it "Multiplying a matrix by the identity matrix" $ do
    let a = fromList 4 4 [0, 1, 2, 4, 1, 2, 4, 8, 2, 4, 8, 16, 4, 8, 16, 32]
    a * (one 4) `shouldBe` a
  it "Multiplying the identity matrix by a tuple" $ do
    let a = tuple 1 2 3 4
    (one 4) *^ a `shouldBe` a

  it "Transposing a matrix" $ do
    let a = fromList 4 4 [0, 9, 3, 0, 9, 8, 0, 8, 1, 8, 5, 3, 0, 0, 5, 8]
    transpose a
      `shouldBe` fromList 4 4 [0, 9, 1, 0, 9, 8, 8, 0, 3, 0, 5, 5, 0, 8, 3, 8]
  it "Transposing the identity matrix" $ do
    transpose (one 4) `shouldBe` (one 4)

  it "Calculating the determinant of a 2x2 matrix" $ do
    let a = fromList 2 2 [1, 5, -3, 2]
    determinant a `shouldBe` 17

  it "A submatrix of a 3x3 matrix is a 2x2 matrix" $ do
    let a = fromList 3 3 [1, 5, 0, -3, 2, 7, 0, 6, -3]
    submatrix 0 2 a `shouldBe` fromList 2 2 [-3, 2, 0, 6]
  it "A submatrix of a 4x4 matrix is a 3x3 matrix" $ do
    let a = fromList 4 4 [-6, 1, 1, 6, -8, 5, 8, 6, -1, 0, 8, 2, -7, 1, -1, 1]
    submatrix 2 1 a `shouldBe` fromList 3 3 [-6, 1, 6, -8, 8, 6, -7, -1, 1]

  it "Calculating a minor of a 3x3 matrix" $ do
    let a = fromList 3 3 [3, 5, 0, 2, -1, -7, 6, -1, 5]
    let b = submatrix 1 0 a
    determinant b `shouldBe` 25
    minor 1 0 a `shouldBe` 25

  it "Calculating a cofactor of a 3x3 matrix" $ do
    let a = fromList 3 3 [3, 5, 0, 2, -1, -7, 6, -1, 5]
    minor 0 0 a `shouldBe` -12
    cofactor 0 0 a `shouldBe` -12
    minor 1 0 a `shouldBe` 25
    cofactor 1 0 a `shouldBe` -25

  it "Calculating the determinant of a 3x3 matrix" $ do
    let a = fromList 3 3 [1, 2, 6, -5, 8, -4, 2, 6, 4]
    cofactor 0 0 a `shouldBe` 56
    cofactor 0 1 a `shouldBe` 12
    cofactor 0 2 a `shouldBe` -46
    determinant a `shouldBe` -196
  it "Calculating the determinant of a 4x4 matrix" $ do
    let a = fromList 4 4 [-2, -8, 3, 5, -3, 1, 7, 3, 1, 2, -9, 6, -6, 7, 7, -9]
    cofactor 0 0 a `shouldBe` 690
    cofactor 0 1 a `shouldBe` 447
    cofactor 0 2 a `shouldBe` 210
    cofactor 0 3 a `shouldBe` 51
    determinant a `shouldBe` -4071

  it "Testing an invertible matrix for invertibility" $ do
    let a = fromList 4 4 [6, 4, 4, 4, 5, 5, 7, 6, 4, -9, 3, -7, 9, 1, 7, -6]
    determinant a `shouldBe` -2120
  it "Testing a noninvertible matrix for invertibility" $ do
    let a = fromList 4 4 [-4, 2, -2, -3, 9, 6, 2, 6, 0, -5, 1, -5, 0, 0, 0, 0]
    determinant a `shouldBe` 0
  it "Calculating the inverse of a matrix" $ do
    let a = fromList 4 4 [-5, 2, 6, -8, 1, -5, 1, 8, 7, 7, -6, -7, 1, -3, 7, 4]
    let b = fromJust $ inverse a
    determinant a `shouldBe` 532
    cofactor 2 3 a `shouldBe` -160
    (b `at` (3, 2)) `shouldBe` pure (-160 / 532)
    cofactor 3 2 a `shouldBe` 105
    (b `at` (2, 3)) `shouldBe` pure (105 / 532)
    b `shouldBe` fromList
      4
      4
      [ 0.21805
      , 0.45113
      , 0.24060
      , -0.04511
      , -0.80827
      , -1.45677
      , -0.44361
      , 0.52068
      , -0.07895
      , -0.22368
      , -0.05263
      , 0.19737
      , -0.52256
      , -0.81391
      , -0.30075
      , 0.30639
      ]
  it "Multiplying a product by its inverse" $ do
    let a = fromList 4 4 [3, -9, 7, 3, 3, -8, 2, -9, -4, 4, 4, 1, -6, 5, -1, 1]
    let b = fromList 4 4 [8, 2, 2, 2, 3, -1, 7, 0, 7, 0, 5, 4, 6, -2, 0, 5]
    let c = a * b
    ((c *) <$> inverse b) `shouldBe` pure a
