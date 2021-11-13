module RayTracer.Chapter9
  ( main,
    Shape (Plane, Sphere),
    plane,
  )
where

import RayTracer.Chapter5 (Pattern, colorPattern)
import RayTracer.Chapter6 (material, sphere)
import RayTracer.Data.Camera
  ( Camera (transformation),
    camera,
    render,
  )
import RayTracer.Data.Canvas (Canvas)
import RayTracer.Data.Color (color)
import RayTracer.Data.Intersection
  ( Intersection (Intersection),
    intersection,
  )
import RayTracer.Data.Light (pointLight)
import RayTracer.Data.Material (Material (diffuse, pattern_, specular))
import qualified RayTracer.Data.Shape as SS (Shape (localIntersect, localNormalAt, material, transformation))
import qualified RayTracer.Data.Shape.Plane as P (Plane (Plane, material))
import qualified RayTracer.Data.Shape.Sphere as S (Sphere (material, transformation))
import RayTracer.Data.Tuple (point, vector)
import RayTracer.Data.World (World (lights, objects), world)
import RayTracer.Transformation
  ( identity,
    scaling,
    translation,
    viewTransform,
  )
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
                { pattern_ = colorPattern $ color 1 0.9 0.9,
                  specular = 0
                }
          }
    middle =
      Sphere $
        sphere
          { S.transformation = translation (-0.5) 1 0.5,
            S.material =
              material
                { pattern_ = colorPattern $ color 0.1 1 0.5,
                  diffuse = 0.7,
                  specular = 0.3
                }
          }
    right =
      Sphere $
        sphere
          { S.transformation = translation 1.5 0.5 (-0.5) * scaling 0.5 0.5 0.5,
            S.material =
              material
                { pattern_ = colorPattern $ color 0.5 1 0.1,
                  diffuse = 0.7,
                  specular = 0.3
                }
          }
    left =
      Sphere $
        sphere
          { S.transformation = translation (-1.5) 0.33 (-0.75) * scaling 0.33 0.33 0.33,
            S.material =
              material
                { pattern_ = colorPattern $ color 1 0.8 0.1,
                  diffuse = 0.7,
                  specular = 0.3
                }
          }
    world' =
      world
        { objects = [floor, middle, right, left],
          lights = [pointLight (point (-10) 10 (-10)) (color 1 1 1)]
        }
    camera' =
      (camera 320 160 (pi / 3))
        { transformation = viewTransform (point 0 1.5 (-5)) (point 0 1 0) (vector 0 1 0)
        }
    canvas :: Canvas Double
    canvas = render camera' world'
