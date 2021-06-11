module RayTracer.Data.Shape.SphereSpec
  ( spec,
  )
where

import RayTracer.Data.Intersection
import qualified RayTracer.Data.Material as M
import RayTracer.Data.Ray
import qualified RayTracer.Data.Shape as S
import RayTracer.Data.Shape.Sphere
import RayTracer.Data.Tuple
import qualified RayTracer.Spec as RS
import RayTracer.Transformation
import Test.Hspec

spec :: Spec
spec = do
  let r = ray (point 0 0 (-5)) (vector 0 0 1)
  let s = RS.sphere
  it "A ray intersects a sphere at two points" $ do
    let xs = r `S.intersect` s
    length xs `shouldBe` 2
    (t <$> xs) `shouldBe` [4.0, 6.0]
  it "A ray intersects a sphere at a tangent" $ do
    let r = ray (point 0 1 (-5)) (vector 0 0 1)
    let xs = r `S.intersect` s
    length xs `shouldBe` 2
    (t <$> xs) `shouldBe` [5.0, 5.0]
  it "A ray misses a sphere" $ do
    let r = ray (point 0 2 (-5)) (vector 0 0 1)
    let xs = r `S.intersect` s
    length xs `shouldBe` 0
  it "A ray originates inside a sphere" $ do
    let r = ray (point 0 0 0) (vector 0 0 1)
    let xs = r `S.intersect` s
    length xs `shouldBe` 2
    (t <$> xs) `shouldBe` [-1.0, 1.0]
  it "A sphere is behind a ray" $ do
    let r = ray (point 0 0 5) (vector 0 0 1)
    let xs = r `S.intersect` s
    length xs `shouldBe` 2
    (t <$> xs) `shouldBe` [-6.0, -4.0]
  it "Intersect sets the object on the intersection" $ do
    let xs = r `S.intersect` s
    length xs `shouldBe` 2
    (object <$> xs) `shouldBe` [s, s]

  it "A sphere's default transformation" $ do
    transformation s `shouldBe` identity
  it "Changing a sphere's transformation" $ do
    let t = translation 2 3 4
    let s = RS.sphere {transformation = t}
    transformation s `shouldBe` t
  it "Intersectiong a scaled sphere with a ray" $ do
    let s = RS.sphere {transformation = scaling 2 2 2}
    let xs = S.intersect r s
    length xs `shouldBe` 2
    (t <$> xs) `shouldBe` [3, 7]
  it "Intersecting a translated sphere with a ray" $ do
    let s = RS.sphere {transformation = translation 5 0 0}
    let xs = S.intersect r s
    length xs `shouldBe` 0

  it "The normal on a sphere at a point on the x axis" $ do
    let n = s `S.normalAt` point 1 0 0
    n `shouldBe` vector 1 0 0
  it "The normal on a sphere at a point on the y axis" $ do
    let n = s `S.normalAt` point 0 1 0
    n `shouldBe` vector 0 1 0
  it "The normal on a sphere at a point on the z axis" $ do
    let n = s `S.normalAt` point 0 0 1
    n `shouldBe` vector 0 0 1
  it "The normal on a sphere at a nonaxial point" $ do
    let n = s `S.normalAt` point (sqrt 3 / 3) (sqrt 3 / 3) (sqrt 3 / 3)
    n `shouldBe` vector (sqrt 3 / 3) (sqrt 3 / 3) (sqrt 3 / 3)
  it "The normal is a normalized vector" $ do
    let n = s `S.normalAt` point (sqrt 3 / 3) (sqrt 3 / 3) (sqrt 3 / 3)
    n `shouldBe` normalize n
  it "Computing the normal on a translated sphere" $ do
    let s = RS.sphere {transformation = translation 0 1 0}
    let n = s `S.normalAt` point 0 1.70711 (-0.70711)
    n `shouldBe` vector 0 0.70711 (-0.70711)
  it "Computing the normal on a transformed sphere" $ do
    let s = RS.sphere {transformation = scaling 1 0.5 1 * rotationZ (pi / 5)}
    let n = s `S.normalAt` point 0 (sqrt 2 / 2) (- sqrt 2 / 2)
    n `shouldBe` vector 0 0.97014 (-0.24254)
  it "A sphere has a default material" $ do
    material s `shouldBe` RS.material
  it "A sphere may be assigned a material" $ do
    let m = RS.material {M.ambient = 1}
    let s = RS.sphere {material = m}
    material s `shouldBe` m
