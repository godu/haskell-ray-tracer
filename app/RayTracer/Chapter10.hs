module RayTracer.Chapter10
  ( main,
  )
where

import RayTracer.Chapter5 hiding (main)
import RayTracer.Chapter6 hiding (main)
import RayTracer.Chapter9 hiding (main)
import qualified RayTracer.Data.Camera as C
import qualified RayTracer.Data.Canvas as C
import qualified RayTracer.Data.Color as C
import RayTracer.Data.Light
import qualified RayTracer.Data.Material as M
import qualified RayTracer.Data.Matrix as M
import qualified RayTracer.Data.Pattern.CheckersPattern as CP
import qualified RayTracer.Data.Shape.Sphere as S
import qualified RayTracer.Data.Tuple as T
import qualified RayTracer.Data.World as W
import RayTracer.Transformation
import Prelude

main :: [String]
main = [show canvas]
  where
    object =
      Sphere $
        sphere
          { S.transformation = scaling 2 2 2,
            S.material =
              material
                { M.pattern_ =
                    CheckersPattern $
                      ( CP.checkersPattern C.white (C.color 0 0.5 0)
                      )
                        { CP.transformation = scaling 0.125 0.125 0.125
                        },
                  M.diffuse = 0.7,
                  M.specular = 0.3
                }
          }
    world =
      W.world
        { W.objects = [object],
          W.lights = [pointLight (T.point (-10) 10 (-10)) (C.color 1 1 1)]
        }
    camera =
      (C.camera 200 200 (pi / 3))
        { C.transformation =
            viewTransform
              ( rotationY (pi / 6) M.*^ T.point 0 0 (-5)
              )
              (T.point 0 0 0)
              (T.vector 0 1 0)
        }
    canvas :: C.Canvas Double
    canvas = C.render camera world
