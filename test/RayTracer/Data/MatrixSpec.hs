module RayTracer.Data.MatrixSpec
  ( spec
  )
where

import           Prelude                        ( ($)
                                                , (*)
                                                , pure
                                                )
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
                                                )
import           Debug.Trace                    ( trace )

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
    let m = fromList 2 2 [(-3), 5, 1, (-2)]
    m `at` (0, 0) `shouldBe` pure (-3)
    m `at` (0, 1) `shouldBe` pure 5
    m `at` (1, 0) `shouldBe` pure 1
    m `at` (1, 1) `shouldBe` pure (-2)
  it "A 3x3 matrix ought to be representable" $ do
    let m = fromList 3 3 [(-3), 5, 0, 1, (-2), (-7), 0, 1, 1]
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
    let b = fromList 4 4 [(-2), 1, 2, 3, 3, 2, 1, (-1), 4, 3, 6, 5, 1, 2, 7, 8]
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
