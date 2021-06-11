module RayTracer.Data.Shape.PlaneSpec
  ( spec,
  )
where

import RayTracer.Data.Intersection
import RayTracer.Data.Ray
import qualified RayTracer.Data.Shape as S
import RayTracer.Data.Shape.Plane
import RayTracer.Data.Tuple
import qualified RayTracer.Spec as RS
import RayTracer.Transformation
import Test.Hspec

plane :: (Fractional a, RealFrac a) => Plane RS.Pattern a
plane = Plane identity RS.material

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
