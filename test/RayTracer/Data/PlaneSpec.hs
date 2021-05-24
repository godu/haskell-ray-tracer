module RayTracer.Data.PlaneSpec
  ( spec,
  )
where

import RayTracer.Data.Intersection (intersection)
import RayTracer.Data.Plane
  ( plane,
  )
import RayTracer.Data.Ray (ray)
import qualified RayTracer.Data.Shape as S
  ( Shape
      ( localIntersect,
        localNormalAt
      ),
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
  ( ($),
  )

spec :: Spec
spec = do
  it "The normal of a plane is constant everywhere" $ do
    let p = plane
        n1 = p `S.localNormalAt` point 0 0 0
        n2 = p `S.localNormalAt` point 10 0 (-10)
        n3 = p `S.localNormalAt` point (-5) 0 150
    n1 `shouldBe` vector 0 1 0
    n2 `shouldBe` vector 0 1 0
    n3 `shouldBe` vector 0 1 0

  it "Intersect with a ray parallel to the plane" $ do
    let p = plane
        r = ray (point 0 10 0) (vector 0 0 1)
        xs = r `S.localIntersect` p
    xs `shouldBe` []
  it "Intersect with a coplanar ray" $ do
    let p = plane
        r = ray (point 0 0 0) (vector 0 0 1)
        xs = r `S.localIntersect` p
    xs `shouldBe` []

  it "A ray intersecting a plane from above" $ do
    let p = plane
        r = ray (point 0 1 0) (vector 0 (-1) 0)
        xs = r `S.localIntersect` p
    xs `shouldBe` [intersection 1 p]
  it "A ray intersecting a plane from below" $ do
    let p = plane
        r = ray (point 0 (-1) 0) (vector 0 1 0)
        xs = r `S.localIntersect` p
    xs `shouldBe` [intersection 1 p]
