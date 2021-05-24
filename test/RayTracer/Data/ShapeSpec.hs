{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module RayTracer.Data.ShapeSpec
  ( spec,
  )
where

import RayTracer.Data.Intersection
  ( intersection,
    intersections,
  )
import qualified RayTracer.Data.Material as M
  ( Material (ambient),
    material,
  )
import RayTracer.Data.Matrix
  ( Matrix,
  )
import RayTracer.Data.Ray
  ( Ray (Ray),
    ray,
  )
import qualified RayTracer.Data.Shape as S
  ( Shape
      ( intersect,
        localIntersect,
        localNormalAt,
        material,
        normalAt,
        transformation
      ),
  )
import RayTracer.Data.Tuple
  ( Tuple (Tuple),
    magnitude,
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
  ( Double,
    Eq,
    Floating (sqrt),
    Num (negate, (*), (+)),
    Ord,
    Show,
    pi,
    ($),
    (/),
  )

data TestShape a = TestShape
  { transformation :: !(Matrix a),
    material :: !(M.Material a)
  }
  deriving (Show, Eq)

instance (Num a, Floating a, Ord a) => S.Shape TestShape a where
  transformation = transformation
  material = material
  localIntersect (Ray o d) s = intersections [intersection (magnitude o + magnitude d) s]
  localNormalAt _ (Tuple x y z _) = vector x y z

testShape :: TestShape Double
testShape = TestShape identity M.material

spec :: Spec
spec = do
  it "The default transformation" $ do
    let s = testShape
    transformation s `shouldBe` identity
  it "Assigning a transformation" $ do
    let s = testShape {transformation = translation 2 3 4}
    transformation s `shouldBe` translation 2 3 4

  it "The default material" $ do
    let s = testShape
    material s `shouldBe` M.material
  it "Assigning a material" $ do
    let s = testShape {material = M.material {M.ambient = 1}}
    material s `shouldBe` M.material {M.ambient = 1}

  it "The default transformation" $ do
    let s = testShape
    transformation s `shouldBe` identity
  it "Assigning a transformation" $ do
    let s = testShape {transformation = translation 2 3 4}
    transformation s `shouldBe` translation 2 3 4

  it "Intersectiong a scaled shape with a ray" $ do
    let r = ray (point 0 0 (-5)) (vector 0 0 1)
        s = testShape {transformation = scaling 2 2 2}
        xs = r `S.intersect` s
        savedRay = ray (point 0 0 (-2.5)) (vector 0 0 0.5)
    xs `shouldBe` savedRay `S.localIntersect` s
  it "Intersectiong a translated shape with a ray" $ do
    let r = ray (point 0 0 (-5)) (vector 0 0 1)
        s = testShape {transformation = translation 5 0 0}
        xs = r `S.intersect` s
        savedRay = ray (point (-5) 0 (-5)) (vector 0 0 1)
    xs `shouldBe` savedRay `S.localIntersect` s

  it "Computing the normal on a translated shape" $ do
    let s = testShape {transformation = translation 0 1 0}
        n = s `S.normalAt` point 0 1.70711 (-0.70711)
    n `shouldBe` vector 0 0.70711 (-0.70711)
  it "Computing the normal on a transformed shape" $ do
    let s = testShape {transformation = scaling 1 0.5 1 * rotationZ (pi / 5)}
        n = s `S.normalAt` point 0 (sqrt 2 / 2) (negate $ sqrt 2 / 2)
    n `shouldBe` vector 0 0.97014 (-0.24254)
