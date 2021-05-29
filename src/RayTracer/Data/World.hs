module RayTracer.Data.World
  ( World (objects, lights),
    world,
    defaultWorld,
    intersect,
    shadeHit,
    colorAt,
    isShadowed,
  )
where

import Data.Maybe (listToMaybe)
import RayTracer.Data.Color (Color, black, color)
import RayTracer.Data.Intersection (Intersection (t), hit, intersections)
import RayTracer.Data.Intersection.Computations (prepareComputations)
import qualified RayTracer.Data.Intersection.Computations as C (Computations (eyev, normalv, object, overPoint))
import RayTracer.Data.Light (Light (position), pointLight)
import RayTracer.Data.Material (lighting)
import qualified RayTracer.Data.Material as M
  ( diffuse,
    material,
    pattern,
    specular,
  )
import RayTracer.Data.Pattern (colorPattern)
import RayTracer.Data.Ray (Ray, ray)
import RayTracer.Data.Shape (Shape)
import qualified RayTracer.Data.Shape as SS
  ( Shape,
    intersect,
    material,
  )
import RayTracer.Data.Sphere (Sphere)
import qualified RayTracer.Data.Sphere as S
  ( material,
    sphere,
    transformation,
  )
import RayTracer.Data.Tuple (Tuple, magnitude, normalize, point)
import RayTracer.Transformation (scaling)
import Prelude
  ( Bool (False, True),
    Eq,
    Floating,
    Foldable (sum),
    Fractional,
    Maybe (Just, Nothing),
    Num ((-)),
    Ord ((<)),
    RealFrac,
    Show,
    concat,
    maybe,
    return,
    ($),
    (.),
    (<$>),
  )

data World o a = World
  { objects :: ![o a],
    lights :: ![Light a]
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
              { M.pattern = colorPattern $ color 0.8 1.0 0.6,
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
  (Num a, Floating a, Eq a, Ord a, Fractional a, SS.Shape o a, Eq (o a)) =>
  Ray a ->
  World o a ->
  [Intersection a o]
intersect r w = intersections $ concat $ SS.intersect r <$> objects w

shadeHit :: (Num a, Floating a, RealFrac a, SS.Shape o a, Eq (o a)) => World o a -> C.Computations a o -> Color a
shadeHit w c =
  sum $
    ( \light ->
        lighting
          (SS.material $ C.object c)
          light
          (C.overPoint c)
          (C.eyev c)
          (C.normalv c)
          (isShadowed w $ C.overPoint c)
    )
      <$> lights w

colorAt :: (Fractional a, Eq (o a), Floating a, RealFrac a, Shape o a) => World o a -> Ray a -> Color a
colorAt w r =
  maybe
    black
    (\i -> shadeHit w $ prepareComputations i r)
    $ hit $
      r `intersect` w

isShadowed :: (Num a, Floating a, Ord a, Shape o a, Eq (o a)) => World o a -> Tuple a -> Bool
isShadowed w p = case listToMaybe (lights w) of
  Nothing -> True
  Just l -> maybe False ((< distance) . t) h
    where
      v = position l - p
      distance = magnitude v
      direction = normalize v
      r = ray p direction
      intersections = intersect r w
      h = hit intersections
