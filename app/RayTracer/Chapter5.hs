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
                                                , (-)
                                                , (*)
                                                , (/)
                                                , floor
                                                , fromIntegral
                                                , quotRem
                                                , filter
                                                )
import           Data.Maybe                     ( isJust )
import           RayTracer.Data.Canvas          ( canvas )
import           RayTracer.Data.Intersection    ( hit )
import           RayTracer.Data.Tuple           ( Tuple(x, y)
                                                , point
                                                , vector
                                                )
import           RayTracer.Data.Sphere          ( sphere
                                                , setTransformation
                                                , intersect
                                                )
import           RayTracer.Data.Ray             ( Ray(Ray)
                                                , ray
                                                )
import           RayTracer.Transformation       ( translation
                                                , scaling
                                                )
import           RayTracer.Projectile           ( updateCanvas
                                                , fuchsia
                                                )

main :: IO ()
main = do
  writeFile "./.output/chapter-5.ppm" $ show finalCanvas
 where
  width         = 550
  height        = 550
  initialCanvas = canvas (width, height)

  s             = setTransformation
    ( (translation ((fromIntegral width) / 2) ((fromIntegral height) / 2) 0)
    * (scaling (0.4 * (fromIntegral width)) (0.4 * (fromIntegral height)) 1)
    )
    sphere

  rays = (toRay . (`quotRem` height)) <$> [0 .. width * height - 1]
   where
    toRay (x, y) =
      ray (point (fromIntegral x) (fromIntegral y) 0) (vector 0 0 1)

  toPixel (Ray t _) = (floor $ x t, floor $ y t)

  pixels      = toPixel <$> filter (isJust . hit . (`intersect` s)) rays

  finalCanvas = updateCanvas fuchsia initialCanvas pixels
