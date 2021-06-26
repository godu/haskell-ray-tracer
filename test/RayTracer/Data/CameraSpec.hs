module RayTracer.Data.CameraSpec
  ( spec,
  )
where

import qualified RayTracer.Data.Camera as C
import qualified RayTracer.Data.Canvas as CC
import qualified RayTracer.Data.Color as CCC
import qualified RayTracer.Data.Ray as R
import qualified RayTracer.Data.Tuple as T
import RayTracer.Extra
import RayTracer.Spec
import qualified RayTracer.Transformation as T
import Test.Hspec

spec :: Spec
spec = do
  it "Constructing a camera" $ do
    let hs = 160
        vs = 120
        fov = pi / 2
        c = C.camera hs vs fov
    C.hsize c `shouldBe` 160
    C.vsize c `shouldBe` 120
    C.fieldOfView c `shouldBe` pi / 2
    C.transformation c `shouldBe` T.identity
  it "The pixel size for a horizontal canvas" $ do
    let c = C.camera 200 125 (pi / 2)
    C.pixelSize c `shouldSatisfy` (~= 0.01)
  it "The pixel size for a vertical canvas" $ do
    let c = C.camera 125 200 (pi / 2)
    C.pixelSize c `shouldSatisfy` (~= 0.01)

  it "Constructing a ray through the center of the canvas" $ do
    let c = C.camera 201 101 (pi / 2)
        r = C.rayForPixel c (100, 50)
    R.origin <$> r `shouldBe` pure (T.point 0 0 0)
    R.direction <$> r `shouldBe` pure (T.vector 0 0 (-1))
  it "Constructing a ray through a corner of the canvas" $ do
    let c = C.camera 201 101 (pi / 2)
        r = C.rayForPixel c (0, 0)
    R.origin <$> r `shouldBe` pure (T.point 0 0 0)
    R.direction <$> r `shouldBe` pure (T.vector 0.66519 0.33259 (-0.66851))
  it "Constructing a ray when the camera is transformed" $ do
    let c = (C.camera 201 101 (pi / 2)) {C.transformation = T.rotationY (pi / 4) * T.translation 0 (-2) 5}
        r = C.rayForPixel c (100, 50)
    R.origin <$> r `shouldBe` pure (T.point 0 2 (-5))
    R.direction <$> r `shouldBe` pure (T.vector (sqrt 2 / 2) 0 (negate $ sqrt 2 / 2))
  it "Rendering a world with a camera" $ do
    let w = world
        from = T.point 0 0 (-5)
        to = T.point 0 0 0
        up = T.vector 0 1 0
        c = (C.camera 11 11 (pi / 2)) {C.transformation = T.viewTransform from to up}
        image = C.render c w
    CC.at image (5, 5) `shouldBe` CCC.color 0.38066 0.47583 0.2855
