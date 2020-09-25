{-# LANGUAGE TupleSections #-}

module RayTracer.Chapter6
  ( main,
  )
where

import Data.Foldable
  ( Foldable,
    foldr,
  )
import Data.Maybe (maybe)
import RayTracer.Data.Canvas
  ( Canvas,
    canvas,
    replace,
  )
import qualified RayTracer.Data.Color as C
  ( Color,
    black,
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
import RayTracer.Data.Sphere
  ( Sphere (material),
    intersect,
    normalAt,
    sphere,
  )
import qualified RayTracer.Data.Tuple as T
  ( normalize,
    point,
  )
import RayTracer.Projectile (fuchsia)
import Prelude
  ( IO,
    Int,
    filter,
    fromIntegral,
    otherwise,
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
  writeFile "./.output/chapter-6.ppm" $ show finalCanvas
  where
    rayOrigin = T.point 0 0 (-5)
    wallZ = 10.0
    wallSize = 7.0
    canvasPixels = 100
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
              M.lighting (material $ object intersection) light point eye normal
    finalCanvas = updateCanvas initialCanvas pixels

updateCanvas :: Foldable t => Canvas a -> t ((Int, Int), C.Color a) -> Canvas a
updateCanvas = foldr go where go (pos, color) = replace pos color
