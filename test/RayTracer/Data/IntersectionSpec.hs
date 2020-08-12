module RayTracer.Data.IntersectionSpec
  ( spec,
  )
where

import Data.Maybe (Maybe (Just, Nothing))
import RayTracer.Data.Intersection
  ( Intersection (object, t),
    hit,
    intersection,
    intersections,
  )
import qualified RayTracer.Data.Intersection.Computation as C
  ( Computation
      ( eyev,
        inside,
        normalv,
        object,
        point,
        t
      ),
    prepareComputations,
  )
import RayTracer.Data.Ray (ray)
import RayTracer.Data.Shape
  ( intersect,
  )
import RayTracer.Data.Shape.Sphere
  ( sphere,
  )
import RayTracer.Data.Tuple
  ( point,
    vector,
  )
import Test.Hspec
  ( Spec,
    it,
    shouldBe,
  )
import Prelude
  ( Bool (..),
    length,
    ($),
    (<$>),
  )

spec :: Spec
spec = do
  it "An intersection encapsulates t and object" $ do
    let s = sphere
    let i = intersection 3.5 sphere
    t i `shouldBe` 3.5
    object i `shouldBe` s
  it "Aggregating intersections" $ do
    let s = sphere
    let i1 = intersection 1 sphere
    let i2 = intersection 2 sphere
    let xs = intersections [i2, i1]
    length xs `shouldBe` 2
    (t <$> xs) `shouldBe` [1, 2]

  it "The hit, when all intersections have positive t" $ do
    let s = sphere
    let i1 = intersection 1 s
    let i2 = intersection 2 s
    let xs = intersections [i2, i1]
    hit xs `shouldBe` Just i1
  it "The hit, when some intersections have negative t" $ do
    let s = sphere
    let i1 = intersection (-1) s
    let i2 = intersection 1 s
    let xs = intersections [i2, i1]
    hit xs `shouldBe` Just i2
  it "The hit, when all intersections have negative t" $ do
    let s = sphere
    let i1 = intersection (-2) s
    let i2 = intersection (-1) s
    let xs = intersections [i2, i1]
    hit xs `shouldBe` Nothing
  it "The hit as always the lowest nonnegative intersection" $ do
    let s = sphere
    let i1 = intersection 5 s
    let i2 = intersection 7 s
    let i3 = intersection (-3) s
    let i4 = intersection 2 s
    let xs = intersections [i1, i2, i3, i4]
    hit xs `shouldBe` Just i4

  it "Precomputing the state of an intersection" $ do
    let r = ray (point 0 0 (-5)) (vector 0 0 1)
    let shape = sphere
    let i = intersection 4 shape
    let comps = C.prepareComputations i r
    C.t comps `shouldBe` t i
    C.object comps `shouldBe` object i
    C.point comps `shouldBe` point 0 0 (-1)
    C.eyev comps `shouldBe` vector 0 0 (-1)
    C.normalv comps `shouldBe` vector 0 0 (-1)
  it "The hit, when an intersection occurs on the outside" $ do
    let r = ray (point 0 0 (-5)) (vector 0 0 1)
    let shape = sphere
    let i = intersection 4 shape
    let comps = C.prepareComputations i r
    C.inside comps `shouldBe` False
  it "The hit, when an intersection occurs on the inside" $ do
    let r = ray (point 0 0 0) (vector 0 0 1)
    let shape = sphere
    let i = intersection 1 shape
    let comps = C.prepareComputations i r
    C.point comps `shouldBe` point 0 0 1
    C.eyev comps `shouldBe` vector 0 0 (-1)
    C.inside comps `shouldBe` True
    C.normalv comps `shouldBe` vector 0 0 (-1)
