module RayTracer.Data.World
  ( World (objects, lights),
    world,
    defaultWorld,
    intersect,
    shadeHit,
  )
where

import Data.List.Ordered (mergeAll)
import Debug.Trace (traceShowId)
import RayTracer.Data.Color (Color, color)
import RayTracer.Data.Intersection (Intersection)
import qualified RayTracer.Data.Intersection.Computations as C (Computations (eyev, normalv, object, point))
import RayTracer.Data.Light (Light, pointLight)
import RayTracer.Data.Material (lighting)
import qualified RayTracer.Data.Material as M
  ( color,
    diffuse,
    material,
    specular,
  )
import RayTracer.Data.Ray (Ray)
import qualified RayTracer.Data.Shape as SS
  ( Shape,
    intersect,
    material,
  )
import qualified RayTracer.Data.Sphere as S
  ( Sphere,
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
    Foldable (sum),
    Fractional,
    Maybe (Nothing),
    Num,
    Ord,
    RealFrac,
    Show,
    concatMap,
    return,
    ($),
    (<$>),
  )

data World a = World
  { objects :: [S.Sphere a],
    lights :: [Light a]
  }
  deriving (Show, Eq)

world :: World a
world = World [] []

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
intersect r w = mergeAll $ SS.intersect r <$> objects w

shadeHit :: (Num a, Floating a, RealFrac a, SS.Shape o a, Show a) => World a -> C.Computations a o -> Color a
shadeHit w c =
  sum $
    ( \light ->
        lighting
          (SS.material $ C.object c)
          light
          (C.point c)
          (C.eyev c)
          (C.normalv c)
    )
      <$> lights w