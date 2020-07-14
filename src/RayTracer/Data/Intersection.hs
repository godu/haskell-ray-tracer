module RayTracer.Data.Intersection
  ( Intersection(t, object)
  , intersection
  , intersections
  , hit
  )
where

import           Prelude                        ( Eq
                                                , Show
                                                , Num
                                                , Ord
                                                , (.)
                                                , (>)
                                                , id
                                                )
import           Data.Maybe                     ( Maybe(Nothing) )
import           Data.List                      ( find
                                                , sortOn
                                                )

data Intersection a o = Intersection { t :: a, object :: o a } deriving (Show, Eq)

intersection :: a -> o a -> Intersection a o
intersection = Intersection

intersections :: (Ord a) => [Intersection a o] -> [Intersection a o]
intersections = sortOn t

hit :: (Ord a, Num a) => [Intersection a o] -> Maybe (Intersection a o)
hit = find ((> 0) . t)
