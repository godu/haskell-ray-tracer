module RayTracer.Chapter2
  ( main,
  )
where

import qualified RayTracer.Data.Canvas as C
import qualified RayTracer.Data.Tuple as T
import RayTracer.Projectile

main :: [String]
main = [show finalCanvas]
  where
    width = 900
    height = 550
    initialCanvas = C.canvas (width, height)

    gravity = T.vector 0 (-0.1) 0
    wind = T.vector (-0.01) 0 0
    environment = Environment gravity wind

    position = T.point 0 1 0
    velocity = (T.*^ 11.25) $ T.normalize $ T.vector 1 1.8 0
    projectile = Projectile position velocity

    toPixel (Projectile position _) =
      (floor $ T.x position, height - floor (T.y position))

    finalCanvas =
      updateCanvas fuchsia initialCanvas (toPixel <$> fire environment projectile)
