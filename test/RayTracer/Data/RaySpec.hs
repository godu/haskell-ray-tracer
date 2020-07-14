module RayTracer.Data.RaySpec
  ( spec
  )
where

import           Prelude                        ( ($) )
import           RayTracer.Data.Tuple           ( point
                                                , vector
                                                )
import           RayTracer.Data.Ray             ( ray
                                                , direction
                                                , origin
                                                , position
                                                )
import           Test.Hspec                     ( Spec
                                                , it
                                                , shouldBe
                                                )

spec :: Spec
spec = do
  it "Creating and querying a ray" $ do
    let o = point 1 2 3
    let d = vector 4 5 6
    let r = ray o d
    origin r `shouldBe` o
    direction r `shouldBe` d
  it "Computing a point from a distance" $ do
    let r = ray (point 2 3 4) (vector 1 0 0)
    position 0 r `shouldBe` point 2 3 4
    position 1 r `shouldBe` point 3 3 4
    position (-1) r `shouldBe` point 1 3 4
    position 2.5 r `shouldBe` point 4.5 3 4
