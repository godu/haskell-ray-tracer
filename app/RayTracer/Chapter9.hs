{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module RayTracer.Chapter9
  ( main,
  )
where

import qualified RayTracer.Data.Camera as C (camera, render, transformation)
import qualified RayTracer.Data.Color as C
  ( color,
  )
import RayTracer.Data.Intersection
  ( Intersection (Intersection),
    intersection,
  )
import RayTracer.Data.Light (pointLight)
import qualified RayTracer.Data.Material as M
  ( Material (diffuse, pattern, specular),
    material,
  )
import RayTracer.Data.Pattern
  ( colorPattern,
  )
import qualified RayTracer.Data.Plane as P
  ( Plane
      ( material
      ),
    plane,
  )
import qualified RayTracer.Data.Shape as SS
  ( Shape
      ( localIntersect,
        localNormalAt,
        material,
        transformation
      ),
  )
import qualified RayTracer.Data.Sphere as S
  ( Sphere (material, transformation),
    sphere,
  )
import qualified RayTracer.Data.Tuple as T
  ( point,
    vector,
  )
import qualified RayTracer.Data.World as W (World (lights, objects), world)
import RayTracer.Transformation
  ( scaling,
    translation,
    viewTransform,
  )
import Prelude
  ( Eq,
    Floating (pi),
    Num,
    Ord,
    Show,
    String,
    show,
    ($),
    (*),
    (/),
    (<$>),
  )

data Shape a = Plane (P.Plane a) | Sphere (S.Sphere a) deriving (Show, Eq)

instance (Num a, Floating a, Ord a) => SS.Shape Shape a where
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

main :: [String]
main = [show canvas]
  where
    floor =
      Plane $
        P.plane
          { P.material =
              M.material
                { M.pattern = colorPattern $ C.color 1 0.9 0.9,
                  M.specular = 0
                }
          }
    middle =
      Sphere $
        S.sphere
          { S.transformation = translation (-0.5) 1 0.5,
            S.material =
              M.material
                { M.pattern = colorPattern $ C.color 0.1 1 0.5,
                  M.diffuse = 0.7,
                  M.specular = 0.3
                }
          }
    right =
      Sphere $
        S.sphere
          { S.transformation = translation 1.5 0.5 (-0.5) * scaling 0.5 0.5 0.5,
            S.material =
              M.material
                { M.pattern = colorPattern $ C.color 0.5 1 0.1,
                  M.diffuse = 0.7,
                  M.specular = 0.3
                }
          }
    left =
      Sphere $
        S.sphere
          { S.transformation = translation (-1.5) 0.33 (-0.75) * scaling 0.33 0.33 0.33,
            S.material =
              M.material
                { M.pattern = colorPattern $ C.color 1 0.8 0.1,
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
