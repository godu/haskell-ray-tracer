module RayTracer.Data.CameraSpec
  ( spec,
  )
where

import RayTracer.Data.Camera
  ( Camera (fieldOfView, hsize, transformation, vsize),
    camera,
    pixelSize,
    rayForPixel,
    render,
  )
import RayTracer.Data.Canvas (at)
import RayTracer.Data.Color (color)
import RayTracer.Data.Ray (Ray (direction, origin), ray)
import RayTracer.Data.Tuple (point, vector)
import RayTracer.Data.World (defaultWorld)
import RayTracer.Extra ((~=))
import RayTracer.Transformation
  ( identity,
    rotationY,
    translation,
    viewTransform,
  )
import Test.Hspec
  ( Spec,
    it,
    shouldBe,
    shouldSatisfy,
  )
import Prelude
  ( Bool (True),
    Double,
    Num ((*)),
    negate,
    pi,
    return,
    sqrt,
    ($),
    (/),
    (<$>),
  )

spec :: Spec
spec = do
  it "Constructing a camera" $ do
    let hs = 160
        vs = 120
        fov = pi / 2
        c = camera hs vs fov
    hsize c `shouldBe` 160
    vsize c `shouldBe` 120
    fieldOfView c `shouldBe` pi / 2
    transformation c `shouldBe` identity
  it "The pixel size for a horizontal canvas" $ do
    let c = camera 200 125 (pi / 2)
    pixelSize c `shouldSatisfy` (~= 0.01)
  it "The pixel size for a vertical canvas" $ do
    let c = camera 125 200 (pi / 2)
    pixelSize c `shouldSatisfy` (~= 0.01)

  it "Constructing a ray through the center of the canvas" $ do
    let c = camera 201 101 (pi / 2)
        r = rayForPixel c (100, 50)
    origin <$> r `shouldBe` return (point 0 0 0)
    direction <$> r `shouldBe` return (vector 0 0 (-1))
  it "Constructing a ray through a corner of the canvas" $ do
    let c = camera 201 101 (pi / 2)
        r = rayForPixel c (0, 0)
    origin <$> r `shouldBe` return (point 0 0 0)
    direction <$> r `shouldBe` return (vector 0.66519 0.33259 (-0.66851))
  it "Constructing a ray when the camera is transformed" $ do
    let c = (camera 201 101 (pi / 2)) {transformation = rotationY (pi / 4) * translation 0 (-2) 5}
        r = rayForPixel c (100, 50)
    origin <$> r `shouldBe` return (point 0 2 (-5))
    direction <$> r `shouldBe` return (vector (sqrt 2 / 2) 0 (negate $ sqrt 2 / 2))
  it "Rendering a world with a camera" $ do
    let w = defaultWorld
        from = point 0 0 (-5)
        to = point 0 0 0
        up = vector 0 1 0
        c = (camera 11 11 (pi / 2)) {transformation = viewTransform from to up}
        image = render c w
    at image (5, 5) `shouldBe` color 0.38066 0.47583 0.2855