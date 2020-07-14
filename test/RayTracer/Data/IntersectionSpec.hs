module RayTracer.Data.IntersectionSpec
  ( spec
  )
where

import           Prelude                        ( ($)
                                                , (<$>)
                                                , length
                                                )
import           Data.Maybe                     ( Maybe(Just, Nothing) )
import           RayTracer.Data.Tuple           ( point
                                                , vector
                                                )
import           RayTracer.Data.Sphere          ( sphere
                                                , intersect
                                                )
import           RayTracer.Data.Ray             ( ray )
import           RayTracer.Data.Intersection    ( Intersection(t, object)
                                                , intersection
                                                , intersections
                                                , hit
                                                )
import           Test.Hspec                     ( Spec
                                                , it
                                                , shouldBe
                                                )


spec :: Spec
spec = do
  it "An intersection encapsulates t and object" $ do
    let s = sphere
    let i = intersection 3.5 sphere
    (t i) `shouldBe` 3.5
    (object i) `shouldBe` s
  it "Aggregating intersections" $ do
    let s  = sphere
    let i1 = intersection 1 sphere
    let i2 = intersection 2 sphere
    let xs = intersections [i2, i1]
    length xs `shouldBe` 2
    (t <$> xs) `shouldBe` [1, 2]

  it "The hit, when all intersections have positive t" $ do
    let s  = sphere
    let i1 = intersection 1 s
    let i2 = intersection 2 s
    let xs = intersections [i2, i1]
    hit xs `shouldBe` Just i1
  it "The hit, when some intersections have negative t" $ do
    let s  = sphere
    let i1 = intersection (-1) s
    let i2 = intersection 1 s
    let xs = intersections [i2, i1]
    hit xs `shouldBe` Just i2
  it "The hit, when all intersections have negative t" $ do
    let s  = sphere
    let i1 = intersection (-2) s
    let i2 = intersection (-1) s
    let xs = intersections [i2, i1]
    hit xs `shouldBe` Nothing
  it "The hit as always the lowest nonnegative intersection" $ do
    let s  = sphere
    let i1 = intersection 5 s
    let i2 = intersection 7 s
    let i3 = intersection (-3) s
    let i4 = intersection 2 s
    let xs = intersections [i1, i2, i3, i4]
    hit xs `shouldBe` Just i4
