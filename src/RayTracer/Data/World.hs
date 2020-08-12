module RayTracer.Data.World
  ( World (lights, objects),
    world,
    defaultWorld,
    intersectWorld,
  )
where

import RayTracer.Data.Color (color)
import RayTracer.Data.Intersection
  ( Intersection,
    intersections,
  )
import RayTracer.Data.Light
  ( Light,
    pointLight,
  )
import qualified RayTracer.Data.Material as M
  ( Material
      ( color,
        diffuse,
        specular
      ),
    material,
  )
import RayTracer.Data.Ray (Ray)
import RayTracer.Data.Shape (intersect)
import RayTracer.Data.Shape.Sphere
  ( Sphere
      ( material,
        transformation
      ),
    sphere,
  )
import RayTracer.Data.Tuple (point)
import RayTracer.Transformation (scaling)
import Prelude
  ( Eq,
    Floating,
    Fractional,
    Num,
    Ord,
    Show,
    concatMap,
    ($),
  )

data World a o = World {objects :: [o a], lights :: [Light a]} deriving (Show, Eq)

world :: (Show a) => World a o
world = World [] []

defaultWorld :: (Num a, Fractional a) => World a Sphere
defaultWorld = World [s1, s2] [l]
  where
    l = pointLight (point (-10) 10 (-10)) (color 1 1 1)
    s1 =
      sphere
        { material =
            M.material
              { M.color = color 0.8 1.0 0.6,
                M.diffuse = 0.7,
                M.specular = 0.2
              }
        }
    s2 = sphere {transformation = scaling 0.5 0.5 0.5}

intersectWorld ::
  (Floating a, Ord a) => Ray a -> World a Sphere -> [Intersection a Sphere]
intersectWorld r w = intersections $ concatMap (intersect r) (objects w)
