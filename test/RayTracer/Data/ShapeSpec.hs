module RayTracer.Data.ShapeSpec
  ( spec,
  )
where

import RayTracer.Data.Intersection
import qualified RayTracer.Data.Material as M
import RayTracer.Data.Matrix
import qualified RayTracer.Data.Pattern as P
import RayTracer.Data.Ray
import qualified RayTracer.Data.Shape as S
import RayTracer.Data.Tuple
import qualified RayTracer.Spec as RS
import RayTracer.Transformation
import Test.Hspec

data TestShape p a = TestShape
  { transformation :: !(Matrix a),
    material :: !(M.Material p a)
  }
  deriving (Show, Eq)

instance (Num a, Floating a, Ord a, P.Pattern p a) => S.Shape TestShape p a where
  transformation = transformation
  material = material
  localIntersect (Ray o d) s = intersections [intersection (magnitude o + magnitude d) s]
  localNormalAt _ (Tuple x y z _) = vector x y z

testShape :: TestShape RS.Pattern Double
testShape = TestShape identity RS.material

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
    material s `shouldBe` RS.material
  it "Assigning a material" $ do
    let s = testShape {material = RS.material {M.ambient = 1}}
    material s `shouldBe` RS.material {M.ambient = 1}

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
