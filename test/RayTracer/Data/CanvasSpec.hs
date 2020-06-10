module RayTracer.Data.CanvasSpec
    ( spec
    )
where

import           Prelude                        ( Real
                                                , ($)
                                                , (+)
                                                , (-)
                                                , (/)
                                                , (*)
                                                )
import           Data.Foldable                  ( traverse_ )
import           Test.Hspec                     ( Spec
                                                , it
                                                , shouldBe
                                                )
import           RayTracer.Data.Canvas          ( canvas
                                                , width
                                                , height
                                                , pixels
                                                , at
                                                , replace
                                                )
import           RayTracer.Data.Color           ( color )

spec :: Spec
spec = do
    it "Creating a canvas" $ do
        let actual = canvas 10 20
        width actual `shouldBe` 10
        height actual `shouldBe` 20
        traverse_ (`shouldBe` color 0 0 0) $ pixels actual
    it "Writing pixels to a canvas" $ do
        let initial = canvas 10 20
        let red     = color 1 0 0
        let actual  = (2, 3) `replace` red $ initial
        actual `at` (2, 3) `shouldBe` red
