{-# LANGUAGE TupleSections #-}

module RayTracer.Chapter6
  ( main,
  )
where

import Data.Maybe (maybe)
import RayTracer.Data.Canvas (bulk, canvas)
import qualified RayTracer.Data.Color as C
  ( black,
    color,
  )
import RayTracer.Data.Intersection
  ( Intersection (object, t),
    hit,
  )
import RayTracer.Data.Light (pointLight)
import qualified RayTracer.Data.Material as M
  ( Material (color),
    lighting,
    material,
  )
import qualified RayTracer.Data.Ray as R
  ( Ray (direction),
    position,
    ray,
  )
import RayTracer.Data.Shape
  ( intersect,
    normalAt,
  )
import RayTracer.Data.Sphere
  ( Sphere (material),
    sphere,
  )
import qualified RayTracer.Data.Tuple as T
  ( normalize,
    point,
  )
import RayTracer.Projectile (fuchsia)
import Prelude
  ( Bool (False),
    String,
    fromIntegral,
    quotRem,
    show,
    ($),
    (*),
    (+),
    (-),
    (.),
    (/),
    (<$>),
  )

main :: [String]
main = [show finalCanvas]
  where
    rayOrigin = T.point 0 0 (-5)
    wallZ = 10.0
    wallSize = 7.0
    canvasPixels = 200
    pixelSize = wallSize / fromIntegral canvasPixels
    half = wallSize / 2
    initialCanvas = canvas (canvasPixels, canvasPixels)
    shape = sphere {material = M.material {M.color = fuchsia}}
    light = pointLight (T.point (-10) 10 (-10)) (C.color 1 1 1)

    pixels =
      cast . (`quotRem` canvasPixels) <$> [0 .. canvasPixels * canvasPixels - 1]

    cast (x, y) = ((x, y),) $ maybe C.black computeColor intersection
      where
        worldX = (- half) + pixelSize * fromIntegral x
        worldY = half - pixelSize * fromIntegral y
        position = T.point worldX worldY wallZ
        ray = R.ray rayOrigin $ T.normalize (position - rayOrigin)
        xs = ray `intersect` shape

        intersection = hit xs

        computeColor intersection = color
          where
            point = R.position (t intersection) ray
            normal = object intersection `normalAt` point
            eye = - (R.direction ray)
            color =
              M.lighting (material $ object intersection) light point eye normal False
    finalCanvas = bulk initialCanvas pixels
