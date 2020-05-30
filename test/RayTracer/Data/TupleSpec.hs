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
  it "vectore() creates tuples with w=1" $ do
    let actual = vector 4 (-4) 3
    actual `shouldBe` tuple 4 (-4) 3 0
