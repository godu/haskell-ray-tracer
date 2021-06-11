module RayTracer.Chapter4
  ( main,
  )
where

import qualified RayTracer.Data.Canvas as C
import qualified RayTracer.Data.Matrix as M
import qualified RayTracer.Data.Tuple as T
import RayTracer.Projectile
import qualified RayTracer.Transformation as T

main :: [String]
main = [show finalCanvas]
  where
    width = 550
    height = 550
    initialCanvas = C.canvas (width, height)

    zero = T.point 0 0 1
    points =
      ( \i ->
          ( T.translation (1 / 2) 0 (1 / 2)
              * T.scaling (3 / 8) (3 / 8) (3 / 8)
              * T.rotationY (i * pi / 6)
              * T.identity
          )
            M.*^ zero
      )
        <$> [0 .. 11]

    toPixel t =
      (floor $ T.x t * fromIntegral height, floor $ T.z t * fromIntegral width)

    finalCanvas = updateCanvas fuchsia initialCanvas $ toPixel <$> points
