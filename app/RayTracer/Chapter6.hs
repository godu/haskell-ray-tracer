module RayTracer.Chapter6
  ( main,
    material,
    sphere,
  )
where

import RayTracer.Chapter5 hiding (main)
import qualified RayTracer.Data.Canvas as C
import qualified RayTracer.Data.Color as C
import qualified RayTracer.Data.Intersection as I
import qualified RayTracer.Data.Light as L
import qualified RayTracer.Data.Material as M
import qualified RayTracer.Data.Material.Extra as M
import qualified RayTracer.Data.Ray as R
import qualified RayTracer.Data.Shape as S
import qualified RayTracer.Data.Shape.Sphere as SS
import qualified RayTracer.Data.Tuple as T
import RayTracer.Projectile
import RayTracer.Transformation

material :: (RealFrac a) => M.Material Pattern a
material = M.Material (colorPattern C.white) 0.1 0.9 0.9 200.0

sphere :: (RealFrac a) => SS.Sphere Pattern a
sphere = SS.Sphere identity material

main :: [String]
main = [show finalCanvas]
  where
    rayOrigin = T.point 0 0 (-5)
    wallZ = 10.0
    wallSize = 7.0
    canvasPixels = 200
    pixelSize = wallSize / fromIntegral canvasPixels
    half = wallSize / 2
    initialCanvas = C.canvas (canvasPixels, canvasPixels)
    shape = sphere {SS.material = material {M.pattern_ = colorPattern fuchsia}}
    light = L.pointLight (T.point (-10) 10 (-10)) (C.color 1 1 1)

    pixels =
      cast . (`quotRem` canvasPixels) <$> [0 .. canvasPixels * canvasPixels - 1]

    cast (x, y) = ((x, y),) $ maybe C.black computeColor intersection
      where
        worldX = (- half) + pixelSize * fromIntegral x
        worldY = half - pixelSize * fromIntegral y
        position = T.point worldX worldY wallZ
        ray = R.ray rayOrigin $ T.normalize (position - rayOrigin)
        xs = ray `S.intersect` shape

        intersection = I.hit xs

        computeColor intersection = color
          where
            point = R.position (I.t intersection) ray
            normal = I.object intersection `S.normalAt` point
            eye = - (R.direction ray)
            color =
              M.lighting
                (S.material $ I.object intersection)
                (I.object intersection)
                light
                point
                eye
                normal
                False
    finalCanvas = C.bulk initialCanvas pixels
