module RayTracer.Chapter3
  ( main
  )
where

import           Prelude                        ( IO
                                                , ($)
                                                , (*)
                                                , (<$>)
                                                , print
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

main :: IO ()
main = do
  print "What happens when you invert the identity matrix ?"
  print $ inverse $ one 4
  print "one 4"

  print "What do you get when you multiply a matrix by its inverse ?"
  print $ (a *) <$> inverse a
  print "one 4"

  print
    "Is there any difference between the inverse of the transpose of a matrix and the transpose of the inverse ?"
  print $ inverse (transpose a)
  print "transpose <$> inverse a"

  print
    "Remember how multiplying the identity matrix by a tuple gives you the tuple, unchanged ?"
  print $ one 4 *^ v
  print "v"

  print
    "Now, try changin any single element of the identity matrix to a different number, and then multiplying it by a tuple. What happens to the tuple ?"
  print $ (update (0, 1) 4 $ one 4) *^ v
  print "vector 9 2 3"
 where
  a = fromList 4 4 [6, 4, 4, 4, 5, 5, 7, 6, 4, -9, 3, -7, 9, 1, 7, -6]
  v = vector 1 2 3
