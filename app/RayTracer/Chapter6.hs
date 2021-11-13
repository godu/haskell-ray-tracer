module RayTracer.Chapter6
  ( main,
    material,
    sphere,
  )
where

import RayTracer.Chapter5 (Pattern, colorPattern)
import RayTracer.Data.Canvas (Canvas, canvas, imap)
import RayTracer.Data.Color (black, color, white)
import RayTracer.Data.Intersection
  ( Intersection (object, t),
    hit,
  )
import RayTracer.Data.Light (pointLight)
import RayTracer.Data.Material
  ( Material (Material, pattern_),
  )
import RayTracer.Data.Material.Extra as M (lighting)
import qualified RayTracer.Data.Ray as R (Ray (direction), position, ray)
import qualified RayTracer.Data.Shape as S (Shape (intersect, material, normalAt))
import qualified RayTracer.Data.Shape.Sphere as SS (Sphere (Sphere, material))
import RayTracer.Data.Tuple (normalize, point)
import RayTracer.Projectile (fuchsia)
import RayTracer.Transformation (identity)

material :: (RealFrac a) => Material Pattern a
material = Material (colorPattern white) 0.1 0.9 0.9 200.0 0.0

sphere :: (RealFrac a) => SS.Sphere Pattern a
sphere = SS.Sphere identity material

main :: [String]
main = [show finalCanvas]
  where
    rayOrigin = point 0 0 (-5)
    wallZ = 10.0
    wallSize = 7.0
    canvasPixels = 200
    pixelSize = wallSize / fromIntegral canvasPixels
    half = wallSize / 2
    initialCanvas = canvas (canvasPixels, canvasPixels)
    shape = sphere {SS.material = material {pattern_ = colorPattern fuchsia}}
    light = pointLight (point (-10) 10 (-10)) (color 1 1 1)

    cast (x, y) _ = maybe black computeColor intersection
      where
        worldX = (- half) + pixelSize * fromIntegral x
        worldY = half - pixelSize * fromIntegral y
        position = point worldX worldY wallZ
        ray = R.ray rayOrigin $ normalize (position - rayOrigin)
        xs = ray `S.intersect` shape

        intersection = hit xs

        computeColor intersection = color
          where
            point = R.position (t intersection) ray
            normal = object intersection `S.normalAt` point
            eye = - (R.direction ray)
            color =
              lighting
                (S.material $ object intersection)
                (object intersection)
                light
                point
                eye
                normal
                False
    finalCanvas :: Canvas Double
    finalCanvas = imap cast initialCanvas
