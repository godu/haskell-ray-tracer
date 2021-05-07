module RayTracer.Data.Intersection.ComputationsSpec
  ( spec,
  )
where

import qualified RayTracer.Data.Intersection as I
  ( Intersection (object, t),
    intersection,
  )
import RayTracer.Data.Intersection.Computations
  ( Computations (eyev, inside, normalv, object, point, t),
    prepareComputations,
  )
import RayTracer.Data.Ray (ray)
import RayTracer.Data.Sphere
  ( intersect,
    sphere,
  )
import qualified RayTracer.Data.Tuple as T
  ( point,
    vector,
  )
import Test.Hspec
  ( Spec,
    it,
    shouldBe,
  )
import Prelude
  ( Bool (False, True),
    ($),
  )

spec :: Spec
spec = do
  it "Precomputing the state of an intersection" $ do
    let r = ray (T.point 0 0 (-5)) (T.vector 0 0 1)
        shape = sphere
        i = 4 `I.intersection` shape
        comps = prepareComputations i r
    t comps `shouldBe` I.t i
    object comps `shouldBe` I.object i
    point comps `shouldBe` T.point 0 0 (-1)
    eyev comps `shouldBe` T.vector 0 0 (-1)
    normalv comps `shouldBe` T.vector 0 0 (-1)

  it "The hit, when an intersection occurs on the outside" $ do
    let r = ray (T.point 0 0 (-5)) (T.vector 0 0 1)
        shape = sphere
        i = 4 `I.intersection` shape
        comps = prepareComputations i r
    inside comps `shouldBe` False

  it "The hit, when an intersection occurs on the inside" $ do
    let r = ray (T.point 0 0 0) (T.vector 0 0 1)
        shape = sphere
        i = 1 `I.intersection` shape
        comps = prepareComputations i r
    point comps `shouldBe` T.point 0 0 1
    eyev comps `shouldBe` T.vector 0 0 (-1)
    inside comps `shouldBe` True
    normalv comps `shouldBe` T.vector 0 0 (-1)