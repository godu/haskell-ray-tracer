module RayTracer.PuttingItTogether.Chapter3Spec
  ( spec
  )
where

import           Prelude                        ( ($)
                                                , (*)
                                                , (<$>)
                                                )
import           Data.Maybe                     ( fromJust )
import           RayTracer.Data.Tuple           ( vector )
import           RayTracer.Data.Matrix          ( (*^)
                                                , update
                                                , inverse
                                                , one
                                                , fromList
                                                , transpose
                                                )
import           Test.Hspec                     ( Spec
                                                , it
                                                , shouldBe
                                                )

spec :: Spec
spec = do
  it "What happens when you invert the identity matrix ?"
    $          fromJust (inverse $ one 4)
    `shouldBe` one 4
  it "What do you get when you multiply a matrix by its inverse ?" $ do
    let a = fromList 4 4 [6, 4, 4, 4, 5, 5, 7, 6, 4, -9, 3, -7, 9, 1, 7, -6]
    a * fromJust (inverse a) `shouldBe` one 4
  it
      "Is there any difference between the inverse of the transpose of a matrix and the transpose of the inverse ?"
    $ do
        let a =
              fromList 4 4 [6, 4, 4, 4, 5, 5, 7, 6, 4, -9, 3, -7, 9, 1, 7, -6]
        inverse (transpose a) `shouldBe` (transpose <$> inverse a)
  it
      "Remember how multiplying the identity matrix by a tuple gives you the tuple, unchanged ?"
    $ do
        let v = vector 1 2 3
        one 4 *^ v `shouldBe` v
  it
      "Now, try changin any single element of the identity matrix to a different number, and then multiplying it by a tuple. What happens to the tuple ?"
    $ do
        let v = vector 1 2 3
        (update (0, 1) 4 $ one 4) *^ v `shouldBe` vector 9 2 3
