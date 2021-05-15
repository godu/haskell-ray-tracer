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
import qualified RayTracer.Data.Intersection.Computations as C (overPoint, point, prepareComputations)
import RayTracer.Data.Ray (ray)
import RayTracer.Data.Shape
  ( intersect,
  )
import RayTracer.Data.Sphere
  ( Sphere (transformation),
    sphere,
  )
import RayTracer.Data.Tuple
  ( point,
    vector,
    z,
  )
import RayTracer.Extra (epsilon)
import RayTracer.Transformation (translation)
import Test.Hspec
  ( Spec,
    it,
    shouldBe,
    shouldSatisfy,
  )
import Prelude
  ( Ord ((<), (>)),
    length,
    negate,
    ($),
    (/),
    (<$>),
  )

spec :: Spec
spec = do
  it "An intersection encapsulates t and object" $ do
    let s = sphere
        i = intersection 3.5 sphere
    t i `shouldBe` 3.5
    object i `shouldBe` s
  it "Aggregating intersections" $ do
    let s = sphere
        i1 = intersection 1 sphere
        i2 = intersection 2 sphere
        xs = intersections [i2, i1]
    length xs `shouldBe` 2
    (t <$> xs) `shouldBe` [1, 2]

  it "The hit, when all intersections have positive t" $ do
    let s = sphere
        i1 = intersection 1 s
        i2 = intersection 2 s
        xs = intersections [i2, i1]
    hit xs `shouldBe` Just i1
  it "The hit, when some intersections have negative t" $ do
    let s = sphere
        i1 = intersection (-1) s
        i2 = intersection 1 s
        xs = intersections [i2, i1]
    hit xs `shouldBe` Just i2
  it "The hit, when all intersections have negative t" $ do
    let s = sphere
        i1 = intersection (-2) s
        i2 = intersection (-1) s
        xs = intersections [i2, i1]
    hit xs `shouldBe` Nothing
  it "The hit as always the lowest nonnegative intersection" $ do
    let s = sphere
        i1 = intersection 5 s
        i2 = intersection 7 s
        i3 = intersection (-3) s
        i4 = intersection 2 s
        xs = intersections [i1, i2, i3, i4]
    hit xs `shouldBe` Just i4

  it "The hit should offset the point" $ do
    let r = ray (point 0 0 (-5)) (vector 0 0 1)
        shape = sphere {transformation = translation 0 0 1}
        i = intersection 5 shape
        comps = C.prepareComputations i r
    z (C.overPoint comps) `shouldSatisfy` (< negate (epsilon / 2))
    z (C.point comps) `shouldSatisfy` (> z (C.overPoint comps))