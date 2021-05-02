module RayTracer.Data.World
  ( World (objects, light),
    world,
    defaultWorld,
    intersect,
  )
where

import Data.List.Ordered (mergeAll)
import RayTracer.Data.Color (color)
import RayTracer.Data.Intersection (Intersection)
import RayTracer.Data.Light (Light, pointLight)
import qualified RayTracer.Data.Material as M
  ( color,
    diffuse,
    material,
    specular,
  )
import RayTracer.Data.Ray (Ray)
import qualified RayTracer.Data.Sphere as S
  ( Sphere,
    intersect,
    material,
    sphere,
    transformation,
  )
import RayTracer.Data.Tuple (point)
import RayTracer.Transformation (scaling)
import Prelude
  ( Double,
    Eq,
    Floating,
    Fractional,
    Maybe (Nothing),
    Num,
    Ord,
    Show,
    concatMap,
    return,
    ($),
    (<$>),
  )

data World a = World
  { objects :: [S.Sphere a],
    light :: Maybe (Light a)
  }
  deriving (Show, Eq)

world :: World a
world = World [] Nothing

defaultWorld :: World Double
defaultWorld = World [s1, s2] (return l)
  where
    s1 =
      S.sphere
        { S.material =
            M.material
              { M.color = color 0.8 1.0 0.6,
                M.diffuse = 0.7,
                M.specular = 0.2
              }
        }
    s2 =
      S.sphere
        { S.transformation = scaling 0.5 0.5 0.5
        }
    l = pointLight (point (-10) 10 (-10)) (color 1 1 1)

intersect ::
  (Num a, Floating a, Eq a, Ord a, Fractional a) =>
  Ray a ->
  World a ->
  [Intersection a S.Sphere]
intersect r w = mergeAll $ S.intersect r <$> objects w