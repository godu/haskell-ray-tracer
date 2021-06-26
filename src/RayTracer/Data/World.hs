module RayTracer.Data.World
  ( World (World, objects, lights),
    world,
    intersect,
    shadeHit,
    colorAt,
    isShadowed,
  )
where

import Data.Maybe (listToMaybe)
import RayTracer.Data.Color (Color, black)
import qualified RayTracer.Data.Intersection as I (Intersection (t), hit, intersections)
import RayTracer.Data.Intersection.Computations
  ( Computations (eyev, normalv, object, overPoint),
    prepareComputations,
  )
import RayTracer.Data.Light (Light (position))
import RayTracer.Data.Material.Extra (lighting)
import RayTracer.Data.Ray (Ray, ray)
import qualified RayTracer.Data.Shape as S (Shape (intersect, material))
import RayTracer.Data.Tuple (Tuple, magnitude, normalize)

data World o a = World
  { objects :: ![o a],
    lights :: ![Light a]
  }

instance (Eq (o p a), Ord a, Fractional a) => Eq (World (o p) a) where
  a == b = objects a == objects b && lights a == lights b

instance (Show (o p a), Show a, RealFrac a) => Show (World (o p) a) where
  show a = "World { object=" <> show (objects a) <> ", lights=" <> show (lights a) <> " }"

world :: World o a
world = World [] []

intersect ::
  (Floating a, Ord a, S.Shape o p a) =>
  Ray a ->
  World (o p) a ->
  [I.Intersection (o p) a]
intersect r w = I.intersections $ concat $ S.intersect r <$> objects w

shadeHit :: (Floating a, RealFrac a, S.Shape o p a) => World (o p) a -> Computations (o p) a -> Color a
shadeHit w c =
  sum $
    ( \light ->
        lighting
          (S.material $ object c)
          (object c)
          light
          (overPoint c)
          (eyev c)
          (normalv c)
          (isShadowed w $ overPoint c)
    )
      <$> lights w

colorAt :: (Fractional a, Floating a, RealFrac a, S.Shape o p a) => World (o p) a -> Ray a -> Color a
colorAt w r =
  maybe
    black
    (\i -> shadeHit w $ prepareComputations i r)
    $ I.hit $
      r `intersect` w

isShadowed :: (Floating a, Ord a, S.Shape o p a) => World (o p) a -> Tuple a -> Bool
isShadowed w p = case listToMaybe (lights w) of
  Nothing -> True
  Just l -> maybe False ((< distance) . I.t) h
    where
      v = position l - p
      distance = magnitude v
      direction = normalize v
      r = ray p direction
      intersections = r `intersect` w
      h = I.hit intersections
