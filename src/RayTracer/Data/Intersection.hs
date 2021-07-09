module RayTracer.Data.Intersection
  ( Intersection (Intersection, t, object),
    intersection,
    intersections,
    hit,
  )
where

import Data.List (find, sort)

data Intersection o a = Intersection {t :: !a, object :: !(o a)} deriving (Show, Eq)

instance (Ord a, Eq (o a)) => Ord (Intersection o a) where
  a <= b = t a <= t b

intersection :: a -> o a -> Intersection o a
intersection = Intersection

intersections :: (Ord a, Eq (o a)) => [Intersection o a] -> [Intersection o a]
intersections = sort

hit :: (Ord a, Num a) => [Intersection o a] -> Maybe (Intersection o a)
hit = find ((> 0) . t)
