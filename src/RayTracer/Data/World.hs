module RayTracer.Data.World
  ( World (objects, lights),
    world,
    defaultWorld,
    intersect,
    shadeHit,
    colorAt,
  )
where

import Debug.Trace (traceShow, traceShowId)
import RayTracer.Data.Color (Color, black, color)
import RayTracer.Data.Intersection (Intersection, hit, intersections)
import RayTracer.Data.Intersection.Computations (prepareComputations)
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
import RayTracer.Data.Shape (Shape)
import qualified RayTracer.Data.Shape as SS
  ( Shape,
    intersect,
    material,
  )
import RayTracer.Data.Sphere (Sphere)
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
    Eq ((/=)),
    Floating,
    Foldable (sum),
    Fractional,
    Maybe (Nothing),
    Num,
    Ord,
    RealFrac,
    Semigroup ((<>)),
    Show (show),
    concat,
    maybe,
    return,
    ($),
    (<$>),
  )

data World o a = World
  { objects :: [o a],
    lights :: [Light a]
  }
  deriving (Show, Eq)

world :: World o a
world = World [] []

defaultWorld :: Fractional a => World Sphere a
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
  (Num a, Floating a, Eq a, Ord a, Fractional a, SS.Shape o a, Eq (o a), Show a, Show (o a)) =>
  Ray a ->
  World o a ->
  [Intersection a o]
intersect r w = intersections $ concat $ SS.intersect r <$> objects w

shadeHit :: (Num a, Floating a, RealFrac a, SS.Shape o a) => World o a -> C.Computations a o -> Color a
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

colorAt :: (Fractional a, Eq (o a), Floating a, RealFrac a, Shape o a, Show a, Show (o a)) => World o a -> Ray a -> Color a
colorAt w r =
  maybe
    black
    (\i -> shadeHit w $ prepareComputations i r)
    $ hit $
      r `intersect` w