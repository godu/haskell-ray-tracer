module RayTracer.Data.SphereSpec
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
import           RayTracer.Data.Ray             ( ray )
import           RayTracer.Data.Sphere          ( Sphere(transformation)
                                                , sphere
                                                , intersect
                                                , setTranformation
                                                )
import           RayTracer.Data.Intersection    ( Intersection(t, object) )
import           RayTracer.Transformation       ( identity
                                                , translation
                                                , scaling
                                                )
import           Test.Hspec                     ( Spec
                                                , it
                                                , shouldBe
                                                )


spec :: Spec
spec = do
  it "A ray intersects a sphere at two points" $ do
    let r  = ray (point 0 0 (-5)) (vector 0 0 1)
    let s  = sphere
    let xs = r `intersect` s
    length xs `shouldBe` 2
    (t <$> xs) `shouldBe` [4.0, 6.0]
  it "A ray intersects a sphere at a tangent" $ do
    let r  = ray (point 0 1 (-5)) (vector 0 0 1)
    let s  = sphere
    let xs = r `intersect` s
    length xs `shouldBe` 2
    (t <$> xs) `shouldBe` [5.0, 5.0]
  it "A ray misses a sphere" $ do
    let r  = ray (point 0 2 (-5)) (vector 0 0 1)
    let s  = sphere
    let xs = r `intersect` s
    length xs `shouldBe` 0
  it "A ray originates inside a sphere" $ do
    let r  = ray (point 0 0 0) (vector 0 0 1)
    let s  = sphere
    let xs = r `intersect` s
    length xs `shouldBe` 2
    (t <$> xs) `shouldBe` [-1.0, 1.0]
  it "A sphere is behind a ray" $ do
    let r  = ray (point 0 0 5) (vector 0 0 1)
    let s  = sphere
    let xs = r `intersect` s
    length xs `shouldBe` 2
    (t <$> xs) `shouldBe` [-6.0, -4.0]
  it "Intersect sets the object on the intersection" $ do
    let r  = ray (point 0 0 (-5)) (vector 0 0 1)
    let s  = sphere
    let xs = r `intersect` s
    length xs `shouldBe` 2
    (object <$> xs) `shouldBe` [s, s]

  it "A sphere's default transformation" $ do
    let s = sphere
    transformation s `shouldBe` identity
  it "Changing a sphere's transformation" $ do
    let s = sphere
    let t = translation 2 3 4
    transformation (setTranformation t s) `shouldBe` t
  it "Intersectiong a scaled sphere with a ray" $ do
    let r  = ray (point 0 0 (-5)) (vector 0 0 1)
    let s  = setTranformation (scaling 2 2 2) sphere
    let xs = intersect r s
    length xs `shouldBe` 2
    (t <$> xs) `shouldBe` [3, 7]
  it "Intersecting a translated sphere with a ray" $ do
    let r  = ray (point 0 0 (-5)) (vector 0 0 1)
    let s  = setTranformation (translation 5 0 0) sphere
    let xs = intersect r s
    length xs `shouldBe` 0
