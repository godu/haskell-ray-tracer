module RayTracer.Transformation
    ( translation
    , scaling
    , rotationX
    )
where

import           Prelude                        ( Num
                                                , Floating
                                                , ($)
                                                , cos
                                                , sin
                                                )

import           RayTracer.Data.Matrix          ( Matrix
                                                , one
                                                , update
                                                )

translation :: Num a => a -> a -> a -> Matrix a
translation x y z = update (0, 3) x $ update (1, 3) y $ update (2, 3) z $ one 4

scaling :: Num a => a -> a -> a -> Matrix a
scaling x y z = update (0, 0) x $ update (1, 1) y $ update (2, 2) z $ one 4

rotationX :: Floating a => a -> Matrix a
rotationX r =
    update (1, 1) (cos r)
        $ update (1, 2) (-(sin r))
        $ update (2, 1) (sin r)
        $ update (2, 2) (cos r)
        $ one 4
