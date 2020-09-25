module RayTracer.Data.Intersection
  ( Intersection (t, object),
    intersection,
    intersections,
    hit,
  )
where

import Data.List
  ( find,
    sortOn,
  )
import Data.Maybe (Maybe (Nothing))
import Prelude
  ( Eq,
    Num,
    Ord,
    Show,
    id,
    (.),
    (>),
  )

data Intersection a o = Intersection {t :: a, object :: o a} deriving (Show, Eq)

intersection :: a -> o a -> Intersection a o
intersection = Intersection

intersections :: (Ord a) => [Intersection a o] -> [Intersection a o]
intersections = sortOn t

hit :: (Ord a, Num a) => [Intersection a o] -> Maybe (Intersection a o)
hit = find ((> 0) . t)
