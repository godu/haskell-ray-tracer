module RayTracer.Chapter9
  ( main,
  )
where

import RayTracer.Chapter5 hiding (main)
import RayTracer.Chapter6 hiding (main)
import qualified RayTracer.Data.Camera as C
import qualified RayTracer.Data.Color as C
import RayTracer.Data.Intersection
import RayTracer.Data.Light
import qualified RayTracer.Data.Material as M
import qualified RayTracer.Data.Shape as SS
import qualified RayTracer.Data.Shape.Plane as P
import qualified RayTracer.Data.Shape.Sphere as S
import qualified RayTracer.Data.Tuple as T
import qualified RayTracer.Data.World as W
import RayTracer.Transformation
import Prelude

data Shape p a = Plane (P.Plane p a) | Sphere (S.Sphere p a) deriving (Show, Eq)

instance (Num a, Floating a, Ord a, RealFrac a) => SS.Shape Shape Pattern a where
  transformation (Plane p) = SS.transformation p
  transformation (Sphere s) = SS.transformation s
  material (Plane p) = SS.material p
  material (Sphere s) = SS.material s
  localIntersect r (Plane p) =
    (\(Intersection t p) -> intersection t (Plane p))
      <$> r `SS.localIntersect` p
  localIntersect r (Sphere s) =
    (\(Intersection t s) -> intersection t (Sphere s))
      <$> r `SS.localIntersect` s
  localNormalAt (Plane p) = SS.localNormalAt p
  localNormalAt (Sphere s) = SS.localNormalAt s

plane :: (Fractional a, RealFrac a) => P.Plane Pattern a
plane = P.Plane identity material

main :: [String]
main = [show canvas]
  where
    floor =
      Plane $
        plane
          { P.material =
              material
                { M.pattern_ = colorPattern $ C.color 1 0.9 0.9,
                  M.specular = 0
                }
          }
    middle =
      Sphere $
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
      Sphere $
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
      Sphere $
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
        { W.objects = [floor, middle, right, left],
          W.lights = [pointLight (T.point (-10) 10 (-10)) (C.color 1 1 1)]
        }
    camera =
      (C.camera 320 160 (pi / 3))
        { C.transformation = viewTransform (T.point 0 1.5 (-5)) (T.point 0 1 0) (T.vector 0 1 0)
        }
    canvas = C.render camera world
