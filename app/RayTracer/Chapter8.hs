module RayTracer.Chapter8
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
    sphereMaterial =
      material
        { M.pattern_ = colorPattern (C.color 1 1 1),
          M.ambient = 0.2,
          M.diffuse = 0.8,
          M.specular = 0.3,
          M.shininess = 200
        }

    wristMaterial =
      sphereMaterial
        { M.pattern_ = colorPattern (C.color 0.1 1 1)
        }

    palmMaterial =
      sphereMaterial
        { M.pattern_ = colorPattern (C.color 0.1 0.1 1)
        }

    thumbMaterial =
      sphereMaterial
        { M.pattern_ = colorPattern (C.color 0.1 0.1 1)
        }

    indexMaterial =
      sphereMaterial
        { M.pattern_ = colorPattern (C.color 1 1 0.1)
        }

    middleMaterial =
      sphereMaterial
        { M.pattern_ = colorPattern (C.color 0.1 1 0.1)
        }

    ringMaterial =
      sphereMaterial
        { M.pattern_ = colorPattern (C.color 0.1 1 0.1)
        }

    pinkyMaterial =
      sphereMaterial
        { M.pattern_ = colorPattern (C.color 0.1 0.5 0.1)
        }

    backdrop :: S.Sphere Pattern Double
    backdrop =
      sphere
        { S.material =
            material
              { M.pattern_ = colorPattern (C.color 1 1 1),
                M.ambient = 0,
                M.diffuse = 0.5,
                M.specular = 0
              },
          S.transformation = translation 0 0 20 * scaling 200 200 0.01
        }

    wrist =
      sphere
        { S.material = wristMaterial,
          S.transformation =
            rotationZ (pi / 4)
              * translation (-4) 0 (-21)
              * scaling 3 3 3
        }

    palm =
      sphere
        { S.material = palmMaterial,
          S.transformation = translation 0 0 (-15) * scaling 4 3 3
        }

    thumb =
      sphere
        { S.material = thumbMaterial,
          S.transformation = translation (-2) 2 (-16) * scaling 1 3 1
        }

    index =
      sphere
        { S.material = indexMaterial,
          S.transformation = translation 3 2 (-22) * scaling 3 0.75 0.75
        }

    middle =
      sphere
        { S.material = middleMaterial,
          S.transformation = translation 4 1 (-19) * scaling 3 0.75 0.75
        }

    ring =
      sphere
        { S.material = ringMaterial,
          S.transformation = translation 4 0 (-18) * scaling 3 0.75 0.75
        }

    pinky =
      sphere
        { S.material = pinkyMaterial,
          S.transformation =
            translation 3 (-1.5) (-20)
              * rotationZ (- pi / 10)
              * translation 1 0 0
              * scaling 2.5 0.6 0.6
        }

    world =
      W.world
        { W.objects =
            [ backdrop,
              wrist,
              palm,
              thumb,
              index,
              middle,
              ring,
              pinky
            ],
          W.lights = [L.pointLight (T.point 0 0 (-100)) (C.color 1 1 1)]
        }

    camera =
      (C.camera 400 200 (pi / 6))
        { C.transformation =
            viewTransform (T.point 40 0 (-70)) (T.point 0 0 (-5)) (T.vector 0 1 0)
        }

    canvas :: C.Canvas Double
    canvas = C.render camera world
