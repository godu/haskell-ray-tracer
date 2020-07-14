module RayTracer.Data.IntersectionSpec
  ( spec
  )
where

import           Prelude                        ( ($)
                                                , (<$>)
                                                , length
                                                )
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
    let xs = intersections i1 i2
    length xs `shouldBe` 2
    (t <$> xs) `shouldBe` [1, 2]
