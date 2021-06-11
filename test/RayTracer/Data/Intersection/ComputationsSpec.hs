module RayTracer.Data.Intersection.ComputationsSpec
  ( spec,
  )
where

import qualified RayTracer.Data.Intersection as I
import RayTracer.Data.Intersection.Computations
import qualified RayTracer.Data.Ray as R
import qualified RayTracer.Data.Tuple as T
import RayTracer.Spec
import Test.Hspec

spec :: Spec
spec = do
  it "Precomputing the state of an intersection" $ do
    let r = R.ray (T.point 0 0 (-5)) (T.vector 0 0 1)
        shape = sphere
        i = 4 `I.intersection` shape
        comps = prepareComputations i r
    t comps `shouldBe` I.t i
    object comps `shouldBe` I.object i
    point comps `shouldBe` T.point 0 0 (-1)
    eyev comps `shouldBe` T.vector 0 0 (-1)
    normalv comps `shouldBe` T.vector 0 0 (-1)

  it "The hit, when an intersection occurs on the outside" $ do
    let r = R.ray (T.point 0 0 (-5)) (T.vector 0 0 1)
        shape = sphere
        i = 4 `I.intersection` shape
        comps = prepareComputations i r
    comps `shouldNotSatisfy` inside

  it "The hit, when an intersection occurs on the inside" $ do
    let r = R.ray (T.point 0 0 0) (T.vector 0 0 1)
        shape = sphere
        i = 1 `I.intersection` shape
        comps = prepareComputations i r
    point comps `shouldBe` T.point 0 0 1
    eyev comps `shouldBe` T.vector 0 0 (-1)
    comps `shouldSatisfy` inside
    normalv comps `shouldBe` T.vector 0 0 (-1)
