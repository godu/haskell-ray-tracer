module RayTracer.Data.Intersection
  ( Intersection (Intersection, t, object),
    intersection,
    intersections,
    hit,
  )
where

import Data.List
  ( find,
    sort,
  )
import Data.Maybe (Maybe)
import Prelude
  ( Eq,
    Num,
    Ord,
    Show,
    (.),
    (<=),
    (>),
  )

data Intersection a o = Intersection {t :: !a, object :: !(o a)} deriving (Show, Eq)

instance (Ord a, Eq (o a)) => Ord (Intersection a o) where
  a <= b = t a <= t b

intersection :: a -> o a -> Intersection a o
intersection = Intersection

intersections :: (Ord a, Eq (o a)) => [Intersection a o] -> [Intersection a o]
intersections = sort

hit :: (Ord a, Num a) => [Intersection a o] -> Maybe (Intersection a o)
hit = find ((> 0) . t)
