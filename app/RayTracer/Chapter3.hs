module RayTracer.Chapter3
  ( main,
  )
where

import RayTracer.Data.Matrix
  ( fromList,
    inverse,
    one,
    transpose,
    update,
    (*^),
  )
import RayTracer.Data.Tuple (vector)

main :: [String]
main =
  [ "What happens when you invert the identity matrix ?",
    show $ inverse $ one 4,
    "one 4",
    "What do you get when you multiply a matrix by its inverse ?",
    show $ (a *) <$> inverse a,
    "one 4",
    "Is there any difference between the inverse of the transpose of a matrix and the transpose of the inverse ?",
    show $ inverse (transpose a),
    "transpose <$> inverse a",
    "Remember how multiplying the identity matrix by a tuple gives you the tuple, unchanged ?",
    show $ one 4 *^ v,
    "v",
    "Now, try changin any single element of the identity matrix to a different number, and then multiplying it by a tuple. What happens to the tuple ?",
    show $ update (0, 1) 4 (one 4) *^ v,
    "vector 9 2 3"
  ]
  where
    a = fromList 4 4 [6, 4, 4, 4, 5, 5, 7, 6, 4, -9, 3, -7, 9, 1, 7, -6]
    v = vector 1 2 3
