module RayTracer.Chapter5
  ( main
  )
where

import           Prelude                        ( IO
                                                , show
                                                , writeFile
                                                , ($)
                                                , (<$>)
                                                , (.)
                                                , (+)
                                                , (-)
                                                , (*)
                                                , (/)
                                                , fromIntegral
                                                , quotRem
                                                , filter
                                                )
import           Data.Maybe                     ( isJust )
import           RayTracer.Data.Canvas          ( canvas )
import           RayTracer.Data.Intersection    ( hit )
import           RayTracer.Data.Tuple           ( point
                                                , normalize
                                                )
import           RayTracer.Data.Sphere          ( sphere
                                                , intersect
                                                )
import           RayTracer.Data.Ray             ( ray )
import           RayTracer.Projectile           ( updateCanvas
                                                , fuchsia
                                                )

main :: IO ()
main = do
  writeFile "./.output/chapter-5.ppm" $ show finalCanvas
 where
  rayOrigin     = point 0 0 (-5)
  wallZ         = 10.0
  wallSize      = 7.0
  canvasPixels  = 100
  pixelSize     = wallSize / fromIntegral canvasPixels
  half          = wallSize / 2
  initialCanvas = canvas (canvasPixels, canvasPixels)
  color         = fuchsia
  shape         = sphere

  pixels =
    filter isHit
      $   (`quotRem` canvasPixels)
      <$> [0 .. canvasPixels * canvasPixels - 1]

  isHit (x, y) = isJust $ hit xs
   where
    worldX   = (-half) + pixelSize * fromIntegral x
    worldY   = half - pixelSize * fromIntegral y
    position = point worldX worldY wallZ
    r        = ray rayOrigin (normalize (position - rayOrigin))
    xs       = r `intersect` shape

  finalCanvas = updateCanvas fuchsia initialCanvas pixels
