module RayTracer.Data.World
  ( World (World, objects, lights),
    world,
    intersect,
    shadeHit,
    colorAt,
    isShadowed,
  )
where

import Data.Maybe
import RayTracer.Data.Color
import qualified RayTracer.Data.Intersection as I
import RayTracer.Data.Intersection.Computations
import qualified RayTracer.Data.Intersection.Computations as C
import qualified RayTracer.Data.Light as L
import RayTracer.Data.Material.Extra
import RayTracer.Data.Ray
import qualified RayTracer.Data.Shape as SS
import qualified RayTracer.Data.Tuple as T

data World o a = World
  { objects :: ![o a],
    lights :: ![L.Light a]
  }

instance (Eq (o p a), Ord a, Fractional a) => Eq (World (o p) a) where
  a == b = objects a == objects b && lights a == lights b

instance (Show (o p a), Show a, RealFrac a) => Show (World (o p) a) where
  show a = "World { object=" <> show (objects a) <> ", lights=" <> show (lights a) <> " }"

world :: World o a
world = World [] []

intersect ::
  (Num a, Floating a, Eq a, Ord a, Fractional a, SS.Shape o p a, Eq (o p a)) =>
  Ray a ->
  World (o p) a ->
  [I.Intersection (o p) a]
intersect r w = I.intersections $ concat $ SS.intersect r <$> objects w

shadeHit :: (Num a, Floating a, RealFrac a, SS.Shape o p a, Eq (o p a), SS.Shape o p a) => World (o p) a -> C.Computations (o p) a -> Color a
shadeHit w c =
  sum $
    ( \light ->
        lighting
          (SS.material $ C.object c)
          (C.object c)
          light
          (C.overPoint c)
          (C.eyev c)
          (C.normalv c)
          (isShadowed w $ C.overPoint c)
    )
      <$> lights w

colorAt :: (Fractional a, Eq (o p a), Floating a, RealFrac a, SS.Shape o p a) => World (o p) a -> Ray a -> Color a
colorAt w r =
  maybe
    black
    (\i -> shadeHit w $ prepareComputations i r)
    $ I.hit $
      r `intersect` w

isShadowed :: (Num a, Floating a, Ord a, SS.Shape o p a, Eq (o p a)) => World (o p) a -> T.Tuple a -> Bool
isShadowed w p = case listToMaybe (lights w) of
  Nothing -> True
  Just l -> maybe False ((< distance) . I.t) h
    where
      v = L.position l - p
      distance = T.magnitude v
      direction = T.normalize v
      r = ray p direction
      intersections = r `intersect` w
      h = I.hit intersections
