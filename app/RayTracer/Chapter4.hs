module RayTracer.Chapter4
  ( main
  )
where

import           Prelude                        ( IO
                                                , Floating
                                                , Double
                                                , Int
                                                , fromIntegral
                                                , ($)
                                                , (<$>)
                                                , (-)
                                                , (*)
                                                , (/)
                                                , show
                                                , writeFile
                                                , print
                                                , floor
                                                , pi
                                                )
import           RayTracer.Data.Tuple           ( Tuple
                                                , point
                                                , x
                                                , z
                                                )
import           RayTracer.Data.Matrix          ( (*^)
                                                , one
                                                )
import           RayTracer.Data.Canvas          ( canvas )
import           RayTracer.Transformation       ( identity
                                                , rotationY
                                                , scaling
                                                , translation
                                                )
import           RayTracer.Projectile           ( updateCanvas
                                                , fuchsia
                                                )

main :: IO ()
main = do
  writeFile "./.output/chapter-4.ppm" $ show finalCanvas
 where
  width         = 550
  height        = 550
  initialCanvas = canvas (width, height)

  zero          = point 0 0 1
  points =
    (\i ->
        ( translation (1 / 2) 0 (1 / 2)
          * scaling (3 / 8) (3 / 8) (3 / 8)
          * rotationY (i * pi / 6)
          * identity
          )
          *^ zero
      )
      <$> [0 .. 11]

  toPixel t =
    ( floor $ (x t) * (fromIntegral height)
    , floor $ (z t) * (fromIntegral width)
    )

  finalCanvas = updateCanvas fuchsia initialCanvas $ toPixel <$> points
