module RayTracer.Chapter5
  ( main,
  )
where

import Data.Maybe (isJust)
import RayTracer.Data.Canvas (canvas)
import RayTracer.Data.Intersection (hit)
import RayTracer.Data.Ray (ray)
import RayTracer.Data.Shape
  ( intersect,
  )
import RayTracer.Data.Shape.Sphere
  ( sphere,
  )
import RayTracer.Data.Tuple
  ( normalize,
    point,
  )
import RayTracer.Projectile
  ( fuchsia,
    updateCanvas,
  )
import Prelude
  ( IO,
    filter,
    fromIntegral,
    quotRem,
    show,
    writeFile,
    ($),
    (*),
    (+),
    (-),
    (.),
    (/),
    (<$>),
  )

main :: IO ()
main = do
  writeFile "./.output/chapter-5.ppm" $ show finalCanvas
  where
    rayOrigin = point 0 0 (-5)
    wallZ = 10.0
    wallSize = 7.0
    canvasPixels = 100
    pixelSize = wallSize / fromIntegral canvasPixels
    half = wallSize / 2
    initialCanvas = canvas (canvasPixels, canvasPixels)
    color = fuchsia
    shape = sphere

    pixels =
      filter isHit $
        (`quotRem` canvasPixels)
          <$> [0 .. canvasPixels * canvasPixels - 1]

    isHit (x, y) = isJust $ hit xs
      where
        worldX = (- half) + pixelSize * fromIntegral x
        worldY = half - pixelSize * fromIntegral y
        position = point worldX worldY wallZ
        r = ray rayOrigin (normalize (position - rayOrigin))
        xs = r `intersect` shape

    finalCanvas = updateCanvas fuchsia initialCanvas pixels
