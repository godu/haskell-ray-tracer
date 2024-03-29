module RayTracer.Chapter4
  ( main,
  )
where

import RayTracer.Data.Canvas (canvas)
import RayTracer.Data.Matrix ((*^))
import RayTracer.Data.Tuple (Tuple (x, z), point)
import RayTracer.Projectile (fuchsia, updateCanvas)
import RayTracer.Transformation
  ( identity,
    rotationY,
    scaling,
    translation,
  )

main :: [String]
main = [show finalCanvas]
  where
    width = 550
    height = 550
    initialCanvas = canvas (width, height)

    zero = point 0 0 1
    points =
      ( \i ->
          ( translation (1 / 2) 0 (1 / 2)
              * scaling (3 / 8) (3 / 8) (3 / 8)
              * rotationY (i * pi / 6)
              * identity
          )
            *^ zero
      )
        <$> [0 .. 11]

    toPixel t =
      (floor $ x t * fromIntegral height, floor $ z t * fromIntegral width)

    finalCanvas = updateCanvas fuchsia initialCanvas $ toPixel <$> points
