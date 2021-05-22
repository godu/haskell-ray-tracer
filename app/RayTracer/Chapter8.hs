module RayTracer.Chapter8
  ( main,
  )
where

import Data.Foldable
  ( Foldable,
  )
import Data.Maybe (maybe)
import qualified RayTracer.Data.Camera as C (camera, render, transformation)
import RayTracer.Data.Canvas
  ( Canvas,
    canvas,
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
  ( Material (color, diffuse, specular),
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
  ( Sphere (material, transformation),
    sphere,
  )
import qualified RayTracer.Data.Tuple as T
  ( normalize,
    point,
    vector,
  )
import qualified RayTracer.Data.World as W (World (lights, objects), world)
import RayTracer.Projectile (fuchsia)
import RayTracer.Transformation (rotationX, rotationY, scaling, translation, viewTransform)
import Prelude
  ( Floating (pi),
    IO,
    Int,
    filter,
    fromIntegral,
    negate,
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
  writeFile "./.output/chapter-8.ppm" $ show canvas
  where
    floor =
      sphere
        { transformation = scaling 10 0.01 10,
          material =
            M.material
              { M.color = C.color 1 0.9 0.9,
                M.specular = 0
              }
        }
    leftWall =
      sphere
        { transformation = translation 0 0 5 * rotationY (negate pi / 4) * rotationX (pi / 2) * scaling 10 0.01 10,
          material =
            M.material
              { M.color = C.color 1 0.9 0.9,
                M.specular = 0
              }
        }
    rightWall =
      sphere
        { transformation = translation 0 0 5 * rotationY (pi / 4) * rotationX (pi / 2) * scaling 10 0.01 10,
          material =
            M.material
              { M.color = C.color 1 0.9 0.9,
                M.specular = 0
              }
        }
    middle =
      sphere
        { transformation = translation (-0.5) 1 0.5,
          material =
            M.material
              { M.color = C.color 0.1 1 0.5,
                M.diffuse = 0.7,
                M.specular = 0.3
              }
        }
    right =
      sphere
        { transformation = translation 1.5 0.5 (-0.5) * scaling 0.5 0.5 0.5,
          material =
            M.material
              { M.color = C.color 0.5 1 0.1,
                M.diffuse = 0.7,
                M.specular = 0.3
              }
        }
    left =
      sphere
        { transformation = translation (-1.5) 0.33 (-0.75) * scaling 0.33 0.33 0.33,
          material =
            M.material
              { M.color = C.color 1 0.8 0.1,
                M.diffuse = 0.7,
                M.specular = 0.3
              }
        }
    world =
      W.world
        { W.objects = [floor, leftWall, rightWall, middle, right, left],
          W.lights = [pointLight (T.point (-10) 10 (-10)) (C.color 1 1 1)]
        }
    camera =
      (C.camera 250 125 (pi / 3))
        { C.transformation = viewTransform (T.point 0 1.5 (-5)) (T.point 0 1 0) (T.vector 0 1 0)
        }
    canvas = C.render camera world
