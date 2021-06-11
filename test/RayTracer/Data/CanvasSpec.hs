module RayTracer.Data.CanvasSpec
  ( spec,
  )
where

import qualified Data.Foldable as F
import RayTracer.Data.Canvas
import RayTracer.Data.Color
import Test.Hspec

spec :: Spec
spec = do
  it "Creating a canvas" $ do
    let actual = canvas (10, 20)
    width actual `shouldBe` 10
    height actual `shouldBe` 20
    F.traverse_ (`shouldBe` color 0 0 0) $ pixels actual
  it "Writing pixels to a canvas" $ do
    let initial = canvas (10, 20)
    let red = color 1 0 0
    let actual = (2, 3) `replace` red $ initial
    actual `at` (2, 3) `shouldBe` red

  it "Constructing the PPM header" $ do
    let actual = canvas (5, 3)
    take 3 (lines $ show actual) `shouldBe` ["P3", "5 3", "255"]

  it "Constructing the PPM pixel data" $ do
    let initial = canvas (5, 3)
    let p1 = color 1.5 0 0
    let p2 = color 0 0.5 0
    let p3 = color (-0.5) 0 1
    let actual =
          bulk
            initial
            [ ((0, 0), p1),
              ((2, 1), p2),
              ((4, 2), p3)
            ]
    drop 3 (take 6 $ lines $ show actual)
      `shouldBe` [ "255 0 0 0 0 0 0 0 0 0 0 0 0 0 0",
                   "0 0 0 0 0 0 0 128 0 0 0 0 0 0 0",
                   "0 0 0 0 0 0 0 0 0 0 0 0 0 0 255"
                 ]
  it "Splitting long lines in PPM files" $ do
    let initial = canvas (10, 2)
    let actual =
          foldr (\p c -> replace p (color 1 0.8 0.6) c) initial $
            concat $
              positions initial
    drop 3 (take 7 $ lines $ show actual)
      `shouldBe` [ "255 204 153 255 204 153 255 204 153 255 204 153 255 204 153 255 204",
                   "153 255 204 153 255 204 153 255 204 153 255 204 153",
                   "255 204 153 255 204 153 255 204 153 255 204 153 255 204 153 255 204",
                   "153 255 204 153 255 204 153 255 204 153 255 204 153"
                 ]
  it "PPM files are terminated by a newline character" $ do
    let actual = canvas (5, 2)
    last (show actual) `shouldBe` '\n'
