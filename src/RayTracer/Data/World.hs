module RayTracer.Data.World
  ( World (World, objects, lights),
    depth,
    world,
    intersect,
    shadeHit,
    colorAt,
    isShadowed,
    reflectedColor,
  )
where

import Data.Maybe (listToMaybe)
import RayTracer.Data.Color (Color, black, (*^))
import qualified RayTracer.Data.Intersection as I (Intersection (t), hit, intersections)
import RayTracer.Data.Intersection.Computations
  ( Computations (eyev, normalv, object, overPoint, reflectv),
    prepareComputations,
  )
import RayTracer.Data.Light (Light (position))
import qualified RayTracer.Data.Material as M
import RayTracer.Data.Material.Extra (lighting)
import RayTracer.Data.Ray (Ray, ray)
import qualified RayTracer.Data.Shape as S (Shape (intersect, material))
import RayTracer.Data.Tuple (Tuple, magnitude, normalize)

depth :: Int
depth = 5

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

shadeHit :: (Floating a, RealFrac a, S.Shape o p a) => Int -> World (o p) a -> Computations (o p) a -> Color a
shadeHit n w c = surface + reflected
  where
    surface =
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
    reflected = reflectedColor n w c

colorAt :: (Floating a, RealFrac a, S.Shape o p a) => Int -> World (o p) a -> Ray a -> Color a
colorAt n w r =
  maybe
    black
    (\i -> shadeHit n w $ prepareComputations i r)
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

reflectedColor :: (S.Shape o p a, Floating a, RealFrac a) => Int -> World (o p) a -> Computations (o p) a -> Color a
reflectedColor n w comps =
  if n == 0 || reflective == 0
    then black
    else color *^ reflective
  where
    reflective = M.reflective $ S.material $ object comps
    reflectRay = ray (overPoint comps) (reflectv comps)
    color = colorAt (n - 1) w reflectRay
