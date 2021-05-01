module RayTracer.Data.World
  ( World (objects, light),
    world,
    defaultWorld,
  )
where

import RayTracer.Data.Color (color)
import RayTracer.Data.Light (Light, pointLight)
import qualified RayTracer.Data.Material as M (color, diffuse, material, specular)
import RayTracer.Data.Sphere (Sphere, material, sphere, transformation)
import RayTracer.Data.Tuple (point)
import RayTracer.Transformation (scaling)
import Prelude (Double, Eq, Maybe (Nothing), Show, return)

data World a = World
  { objects :: [Sphere a],
    light :: Maybe (Light a)
  }
  deriving (Show, Eq)

world :: World a
world = World [] Nothing

defaultWorld :: World Double
defaultWorld = World [s1, s2] (return l)
  where
    s1 =
      sphere
        { material =
            M.material
              { M.color = color 0.8 1.0 0.6,
                M.diffuse = 0.7,
                M.specular = 0.2
              }
        }
    s2 =
      sphere
        { transformation = scaling 0.5 0.5 0.5
        }
    l = pointLight (point (-10) 10 (-10)) (color 1 1 1)