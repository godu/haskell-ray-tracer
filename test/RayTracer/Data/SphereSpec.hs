module RayTracer.Data.SphereSpec
  ( spec,
  )
where

import RayTracer.Data.Intersection (Intersection (object, t))
import qualified RayTracer.Data.Material as M
  ( Material (ambient),
    material,
  )
import RayTracer.Data.Matrix (fromList)
import RayTracer.Data.Ray (ray)
import RayTracer.Data.Sphere
  ( Sphere
      ( material,
        transformation
      ),
    intersect,
    normalAt,
    sphere,
  )
import RayTracer.Data.Tuple
  ( normalize,
    point,
    vector,
  )
import RayTracer.Transformation
  ( identity,
    rotationZ,
    scaling,
    translation,
  )
import Test.Hspec
  ( Spec,
    it,
    shouldBe,
  )
import Prelude
  ( length,
    pi,
    sqrt,
    ($),
    (*),
    (/),
    (<$>),
  )

spec :: Spec
spec = do
  let r = ray (point 0 0 (-5)) (vector 0 0 1)
  let s = sphere
  it "A ray intersects a sphere at two points" $ do
    let xs = r `intersect` s
    length xs `shouldBe` 2
    (t <$> xs) `shouldBe` [4.0, 6.0]
  it "A ray intersects a sphere at a tangent" $ do
    let r = ray (point 0 1 (-5)) (vector 0 0 1)
    let xs = r `intersect` s
    length xs `shouldBe` 2
    (t <$> xs) `shouldBe` [5.0, 5.0]
  it "A ray misses a sphere" $ do
    let r = ray (point 0 2 (-5)) (vector 0 0 1)
    let xs = r `intersect` s
    length xs `shouldBe` 0
  it "A ray originates inside a sphere" $ do
    let r = ray (point 0 0 0) (vector 0 0 1)
    let xs = r `intersect` s
    length xs `shouldBe` 2
    (t <$> xs) `shouldBe` [-1.0, 1.0]
  it "A sphere is behind a ray" $ do
    let r = ray (point 0 0 5) (vector 0 0 1)
    let xs = r `intersect` s
    length xs `shouldBe` 2
    (t <$> xs) `shouldBe` [-6.0, -4.0]
  it "Intersect sets the object on the intersection" $ do
    let xs = r `intersect` s
    length xs `shouldBe` 2
    (object <$> xs) `shouldBe` [s, s]

  it "A sphere's default transformation" $ do
    transformation s `shouldBe` identity
  it "Changing a sphere's transformation" $ do
    let t = translation 2 3 4
    let s = sphere {transformation = t}
    transformation s `shouldBe` t
  it "Intersectiong a scaled sphere with a ray" $ do
    let s = sphere {transformation = scaling 2 2 2}
    let xs = intersect r s
    length xs `shouldBe` 2
    (t <$> xs) `shouldBe` [3, 7]
  it "Intersecting a translated sphere with a ray" $ do
    let s = sphere {transformation = translation 5 0 0}
    let xs = intersect r s
    length xs `shouldBe` 0

  it "The normal on a sphere at a point on the x axis" $ do
    let n = s `normalAt` point 1 0 0
    n `shouldBe` vector 1 0 0
  it "The normal on a sphere at a point on the y axis" $ do
    let n = s `normalAt` point 0 1 0
    n `shouldBe` vector 0 1 0
  it "The normal on a sphere at a point on the z axis" $ do
    let n = s `normalAt` point 0 0 1
    n `shouldBe` vector 0 0 1
  it "The normal on a sphere at a nonaxial point" $ do
    let n = s `normalAt` point (sqrt 3 / 3) (sqrt 3 / 3) (sqrt 3 / 3)
    n `shouldBe` vector (sqrt 3 / 3) (sqrt 3 / 3) (sqrt 3 / 3)
  it "The normal is a normalized vector" $ do
    let n = s `normalAt` point (sqrt 3 / 3) (sqrt 3 / 3) (sqrt 3 / 3)
    n `shouldBe` normalize n
  it "Computing the normal on a translated sphere" $ do
    let s = sphere {transformation = translation 0 1 0}
    let n = s `normalAt` point 0 1.70711 (-0.70711)
    n `shouldBe` vector 0 0.70711 (-0.70711)
  it "Computing the normal on a transformed sphere" $ do
    let s = sphere {transformation = scaling 1 0.5 1 * rotationZ (pi / 5)}
    let n = s `normalAt` point 0 (sqrt 2 / 2) (- sqrt 2 / 2)
    n `shouldBe` vector 0 0.97014 (-0.24254)
  it "A sphere has a default material" $ do
    material s `shouldBe` M.material
  it "A sphere may be assigned a material" $ do
    let m = M.material {M.ambient = 1}
    let s = sphere {material = m}
    material s `shouldBe` m
