module RayTracer.Chapter2
  ( main,
  )
where

import RayTracer.Data.Canvas (canvas)
import RayTracer.Data.Tuple
  ( normalize,
    point,
    vector,
    x,
    y,
    (*^),
  )
import RayTracer.Projectile
  ( Environment (Environment),
    Projectile (Projectile),
    fire,
    fuchsia,
    updateCanvas,
  )
import Prelude
  ( IO,
    floor,
    fmap,
    print,
    show,
    writeFile,
    ($),
    (-),
    (<$>),
  )

main :: IO ()
main = do
  writeFile "./.output/chapter-2.ppm" $ show finalCanvas
  where
    width = 900
    height = 550
    initialCanvas = canvas (width, height)

    gravity = vector 0 (-0.1) 0
    wind = vector (-0.01) 0 0
    environment = Environment gravity wind

    position = point 0 1 0
    velocity = (*^ 11.25) $ normalize $ vector 1 1.8 0
    projectile = Projectile position velocity

    toPixel (Projectile position _) =
      (floor $ x position, height - floor (y position))

    finalCanvas =
      updateCanvas fuchsia initialCanvas (toPixel <$> fire environment projectile)
