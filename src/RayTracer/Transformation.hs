module RayTracer.Transformation
    ( translation
    , scaling
    )
where

import           RayTracer.Data.Matrix          ( Matrix
                                                , one
                                                , update
                                                )

translation :: Num a => a -> a -> a -> Matrix a
translation x y z = update (0, 3) x $ update (1, 3) y $ update (2, 3) z $ one 4

scaling :: Num a => a -> a -> a -> Matrix a
scaling x y z = update (0, 0) x $ update (1, 1) y $ update (2, 2) z $ one 4
