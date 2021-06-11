module RayTracer.Chapter3
  ( main,
  )
where

import qualified RayTracer.Data.Matrix as M
import qualified RayTracer.Data.Tuple as T

main :: [String]
main =
  [ "What happens when you invert the identity matrix ?",
    show $ M.inverse $ M.one 4,
    "one 4",
    "What do you get when you multiply a matrix by its inverse ?",
    show $ (a *) <$> M.inverse a,
    "one 4",
    "Is there any difference between the inverse of the transpose of a matrix and the transpose of the inverse ?",
    show $ M.inverse (M.transpose a),
    "transpose <$> inverse a",
    "Remember how multiplying the identity matrix by a tuple gives you the tuple, unchanged ?",
    show $ M.one 4 M.*^ v,
    "v",
    "Now, try changin any single element of the identity matrix to a different number, and then multiplying it by a tuple. What happens to the tuple ?",
    show $ M.update (0, 1) 4 (M.one 4) M.*^ v,
    "vector 9 2 3"
  ]
  where
    a = M.fromList 4 4 [6, 4, 4, 4, 5, 5, 7, 6, 4, -9, 3, -7, 9, 1, 7, -6]
    v = T.vector 1 2 3
