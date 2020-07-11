module RayTracer.Chapter3
    ( main
    )
where

import           Prelude                        ( IO
                                                , ($)
                                                , (<$>)
                                                , show
                                                , (*)
                                                , print
                                                , floor
                                                , fmap
                                                )
import           RayTracer.Data.Tuple           ( vector )
import           RayTracer.Data.Matrix          ( (*^)
                                                , one
                                                , update
                                                , inverse
                                                , transpose
                                                , fromList
                                                )
import           RayTracer.Data.Extra           ( (~=) )

main :: IO ()
main = do
    print "What happens when you invert the identity matrix ?"
    print $ inverse $ one 4
    print "What do you get when you multiply a matrix by its inverse ?"
    let a = fromList 4 4 [6, 4, 4, 4, 5, 5, 7, 6, 4, -9, 3, -7, 9, 1, 7, -6]
    print $ (a *) <$> inverse a
    print
        "Is there any difference between the inverse of the transpose of a matrix and the transpose of the inverse ?"
    print $ inverse $ transpose a
    print $ transpose <$> inverse a
    print
        "Remember how multiplying the identity matrix by a tuple gives you the tuple, unchanged ?"
    let v = vector 1 2 3
    print $ one 4 *^ v
    print
        "Now, try changin any single element of the identity matrix to a different number, and then multiplying it by a tuple. What happens to the tuple ?"
    print $ (update (4, 1) 4 $ one 4) *^ v
