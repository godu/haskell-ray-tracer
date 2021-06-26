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
-- import qualified RayTracer.Data.Pattern.CheckersPattern as CP
import qualified RayTracer.Data.Pattern.RadialGradientPattern as RGP
import qualified RayTracer.Data.Pattern.RingPattern as RP
import qualified RayTracer.Data.Shape.Plane as P
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
          { S.transformation = scaling 2.5 2.5 2.5,
            S.material =
              material
                { M.pattern_ =
                    -- RadialGradientPattern $ RGP.radialGradientPattern C.white (C.color 0 0.5 0),
                    -- RingPattern $ RP.ringPattern C.white (C.color 0 0.5 0),
                    RadialGradientPattern $
                      (RGP.radialGradientPattern C.white (C.color 0 0.5 0))
                        { RGP.transformation = scaling 0.125 0.125 0.125
                        },
                  M.diffuse = 0.7,
                  M.specular = 0.3
                }
          }
    plan =
      Plane $
        plane
          { P.transformation = rotationX (pi / 2),
            P.material =
              material
                { M.pattern_ =
                    -- RadialGradientPattern $ RGP.radialGradientPattern C.white (C.color 0 0.5 0),
                    -- RingPattern $ RP.ringPattern C.white (C.color 0 0.5 0),
                    RadialGradientPattern $ RGP.radialGradientPattern C.white (C.color 0 0.5 0),
                  M.diffuse = 0.7,
                  M.specular = 0.3
                }
          }
    world =
      W.world
        { W.objects = [plan, object],
          W.lights = [pointLight (T.point (-10) 10 (-10)) (C.color 1 1 1)]
        }
    camera =
      (C.camera 320 320 (pi / 3))
        { C.transformation =
            viewTransform
              (T.point 0 1.5 (-5))
              (T.point 0 1 0)
              (T.vector 0 1 0)
        }
    canvas :: C.Canvas Double
    canvas = C.render camera world
