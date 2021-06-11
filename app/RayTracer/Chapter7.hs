module RayTracer.Chapter7
  ( main,
  )
where

import RayTracer.Chapter5 hiding (main)
import RayTracer.Chapter6 hiding (main)
import qualified RayTracer.Data.Camera as C
import qualified RayTracer.Data.Canvas as C
import qualified RayTracer.Data.Color as C
import qualified RayTracer.Data.Light as L
import qualified RayTracer.Data.Material as M
import qualified RayTracer.Data.Shape.Sphere as S
import qualified RayTracer.Data.Tuple as T
import qualified RayTracer.Data.World as W
import RayTracer.Transformation

main :: [String]
main = [show canvas]
  where
    floor :: S.Sphere Pattern Double
    floor =
      sphere
        { S.transformation = scaling 10 0.01 10,
          S.material =
            material
              { M.pattern_ = colorPattern (C.color 1 0.9 0.9),
                M.specular = 0
              }
        }

    leftWall =
      sphere
        { S.transformation = translation 0 0 5 * rotationY (negate pi / 4) * rotationX (pi / 2) * scaling 10 0.01 10,
          S.material =
            material
              { M.pattern_ = colorPattern (C.color 1 0.9 0.9),
                M.specular = 0
              }
        }

    rightWall =
      sphere
        { S.transformation = translation 0 0 5 * rotationY (pi / 4) * rotationX (pi / 2) * scaling 10 0.01 10,
          S.material =
            material
              { M.pattern_ = colorPattern (C.color 1 0.9 0.9),
                M.specular = 0
              }
        }

    middle =
      sphere
        { S.transformation = translation (-0.5) 1 0.5,
          S.material =
            material
              { M.pattern_ = colorPattern $ C.color 0.1 1 0.5,
                M.diffuse = 0.7,
                M.specular = 0.3
              }
        }

    right =
      sphere
        { S.transformation = translation 1.5 0.5 (-0.5) * scaling 0.5 0.5 0.5,
          S.material =
            material
              { M.pattern_ = colorPattern $ C.color 0.5 1 0.1,
                M.diffuse = 0.7,
                M.specular = 0.3
              }
        }

    left =
      sphere
        { S.transformation = translation (-1.5) 0.33 (-0.75) * scaling 0.33 0.33 0.33,
          S.material =
            material
              { M.pattern_ = colorPattern $ C.color 1 0.8 0.1,
                M.diffuse = 0.7,
                M.specular = 0.3
              }
        }

    world =
      W.world
        { W.objects =
            [ floor,
              leftWall,
              rightWall,
              middle,
              right,
              left
            ],
          W.lights = [L.pointLight (T.point (-10) 10 (-10)) (C.color 1 1 1)]
        }

    camera =
      (C.camera 320 160 (pi / 3))
        { C.transformation = viewTransform (T.point 0 1.5 (-5)) (T.point 0 1 0) (T.vector 0 1 0)
        }

    canvas :: C.Canvas Double
    canvas = C.render camera world
