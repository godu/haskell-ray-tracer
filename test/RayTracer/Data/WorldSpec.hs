module RayTracer.Data.WorldSpec
  ( spec,
  )
where

import Control.Monad (mzero)
import RayTracer.Data.Color (black)
import qualified RayTracer.Data.Color as C
import qualified RayTracer.Data.Intersection as I
import RayTracer.Data.Intersection.Computations
import qualified RayTracer.Data.Light as L
import qualified RayTracer.Data.Material as M
import qualified RayTracer.Data.Ray as R
import qualified RayTracer.Data.Shape.Plane as P
import qualified RayTracer.Data.Shape.Sphere as S
import qualified RayTracer.Data.Tuple as T
import qualified RayTracer.Data.World as W
import RayTracer.Spec
import RayTracer.Transformation
import Test.Hspec

spec :: Spec
spec = do
  it "Creating a world" $ do
    let actual :: W.World (S.Sphere Pattern) Double
        actual = W.world
    W.objects actual `shouldBe` []
    W.lights actual `shouldBe` []

  it "The default world" $ do
    let l = L.pointLight (T.point (-10) 10 (-10)) (C.color 1 1 1)
        s1 =
          sphere
            { S.material =
                material
                  { M.pattern_ = colorPattern $ C.color 0.8 1.0 0.6,
                    M.diffuse = 0.7,
                    M.specular = 0.2
                  }
            }
        s2 =
          sphere
            { S.transformation = scaling 0.5 0.5 0.5
            }
        w = world
    W.lights w `shouldBe` pure l
    W.objects w `shouldContain` [s1]
    W.objects w `shouldContain` [s2]

  it "Intersect a world with a ray" $ do
    let w = world
        r = R.ray (T.point 0 0 (-5)) (T.vector 0 0 1)
        xs = r `W.intersect` w
    fmap I.t xs `shouldBe` [4, 4.5, 5.5, 6]

  it "Shading an intersection" $ do
    let w = world
        r = R.ray (T.point 0 0 (-5)) (T.vector 0 0 1)
        shape = head $ W.objects w
        i = I.intersection 4 shape
        comps = prepareComputations i r
        c = W.shadeHit w comps
    c `shouldBe` C.color 0.38066 0.47583 0.2855
  it "Shading an intersection from the inside" $ do
    let w =
          world
            { W.lights = pure $ L.pointLight (T.point 0 0.25 0) (C.color 1 1 1)
            }
        r = R.ray (T.point 0 0 0) (T.vector 0 0 1)
        shape = W.objects w !! 1
        i = I.intersection 0.5 shape
        comps = prepareComputations i r
        c = W.shadeHit w comps
    c `shouldBe` C.color 0.90498 0.90498 0.90498

  it "The color when a ray missing" $ do
    let w = world
        r = R.ray (T.point 0 0 (-5)) (T.vector 0 1 0)
        c = w `W.colorAt` r
    c `shouldBe` C.color 0 0 0
  it "The color when a hits" $ do
    let w = world
        r = R.ray (T.point 0 0 (-5)) (T.vector 0 0 1)
        c = w `W.colorAt` r
    c `shouldBe` C.color 0.38066 0.47583 0.2855
  it "The color with an intersection behind the ray" $ do
    case W.objects world of
      [s1, s2] ->
        let w =
              world
                { W.objects =
                    [ s1 {S.material = (S.material s1) {M.ambient = 1}},
                      s2 {S.material = (S.material s2) {M.ambient = 1}}
                    ]
                }
            r = R.ray (T.point 0 0 0.75) (T.vector 0 0 (-1))
            c = w `W.colorAt` r
         in colorPattern c `shouldBe` M.pattern_ (S.material s2)
      _ -> mzero

  it "There is no shadow when nothing is collinear with T.point and light" $ do
    let w = world
        p = T.point 0 10 0
    p `shouldNotSatisfy` W.isShadowed w
  it "The shadow when an object is between the T.point and the light" $ do
    let w = world
        p = T.point 10 (-10) 10
    p `shouldSatisfy` W.isShadowed w
  it "There is no shadow when an object is behind the light" $ do
    let w = world
        p = T.point (-20) 20 (-20)
    p `shouldNotSatisfy` W.isShadowed w
  it "There is no shadow when an object is behind the point" $ do
    let w = world
        p = T.point (-2) 2 (-2)
    p `shouldNotSatisfy` W.isShadowed w

  it "shadeHit() is given an intersection in shadow" $ do
    let s1 = sphere
        s2 = sphere {S.transformation = translation 0 0 10}
        w =
          world
            { W.lights = [L.pointLight (T.point 0 0 (-10)) (C.color 1 1 1)],
              W.objects = [s1, s2]
            }
        r = R.ray (T.point 0 0 5) (T.vector 0 0 1)
        i = I.intersection 4 s2
        comps = prepareComputations i r
    W.shadeHit w comps `shouldBe` C.color 0.1 0.1 0.1

  it "The reflected color for a nonreflective material" $ do
    case W.objects world of
      [s1, s2] ->
        let s2' = s2 {S.material = (S.material s2) {M.ambient = 1}}
            w = world {W.objects = [s1, s2']}
            r = R.ray (T.point 0 0 0) (T.vector 0 0 1)
            i = I.intersection 1 s2'
            comps = prepareComputations i r
            color = W.reflectedColor w comps
         in color `shouldBe` black
      _ -> mzero

  it "The reflected color for a reflective material" $ do
    case W.objects world of
      [s1, s2] ->
        let shape =
              Plane $
                ( plane
                    { P.material =
                        (P.material plane)
                          { M.reflective = 0.5
                          },
                      P.transformation = translation 0 (-1) 0
                    }
                )
            w =
              world
                { W.objects = shape : (Sphere <$> [s1, s2])
                }
            r = R.ray (T.point 0 0 (-3)) (T.vector 0 (- sqrt 2 / 2) (sqrt 2 / 2))
            i = I.intersection (sqrt 2) shape
            comps = prepareComputations i r
            color = W.reflectedColor w comps
         in color `shouldBe` C.color 0.19032 0.2379 0.14274
      _ -> mzero
