module RayTracer.Transformation
  ( identity,
    translation,
    scaling,
    rotationX,
    rotationY,
    rotationZ,
    shearing,
    viewTransform,
  )
where

import RayTracer.Data.Matrix (Matrix, fromList, one, update)
import RayTracer.Data.Tuple (Tuple (x, y, z), normalize)

identity :: Num a => Matrix a
identity = one 4

translation :: Num a => a -> a -> a -> Matrix a
translation x y z =
  update (0, 3) x $ update (1, 3) y $ update (2, 3) z identity

scaling :: Num a => a -> a -> a -> Matrix a
scaling x y z = update (0, 0) x $ update (1, 1) y $ update (2, 2) z identity

rotationX :: Floating a => a -> Matrix a
rotationX r =
  update (1, 1) (cos r) $
    update (1, 2) (- (sin r)) $
      update (2, 1) (sin r) $
        update (2, 2) (cos r) identity

rotationY :: Floating a => a -> Matrix a
rotationY r =
  update (0, 0) (cos r) $
    update (0, 2) (sin r) $
      update (2, 0) (- (sin r)) $
        update (2, 2) (cos r) identity

rotationZ :: Floating a => a -> Matrix a
rotationZ r =
  update (0, 0) (cos r) $
    update (0, 1) (- (sin r)) $
      update (1, 0) (sin r) $
        update (1, 1) (cos r) identity

shearing :: Num a => a -> a -> a -> a -> a -> a -> Matrix a
shearing a b c d e f =
  update (0, 1) a $
    update (0, 2) b $
      update (1, 0) c $
        update (1, 2) d $
          update (2, 0) e $
            update (2, 1) f identity

viewTransform :: (Floating a) => Tuple a -> Tuple a -> Tuple a -> Matrix a
viewTransform from to up = orientation * translation (negate $ x from) (negate $ y from) (negate $ z from)
  where
    forward = normalize $ to - from
    left = forward * normalize up
    trueUp = left * forward
    orientation =
      fromList
        4
        4
        [ x left,
          y left,
          z left,
          0,
          x trueUp,
          y trueUp,
          z trueUp,
          0,
          negate $ x forward,
          negate $ y forward,
          negate $ z forward,
          0,
          0,
          0,
          0,
          1
        ]
