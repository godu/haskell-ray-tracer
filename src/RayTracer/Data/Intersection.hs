module RayTracer.Data.Intersection
  ( Intersection(t, object)
  , intersection
  , intersections
  )
where

import           Prelude                        ( Eq
                                                , Show
                                                , Num
                                                )

data Intersection a o = Intersection { t :: a, object :: o a } deriving (Show, Eq)

intersection :: a -> o a -> Intersection a o
intersection = Intersection

intersections :: Intersection a o -> Intersection a o -> [Intersection a o]
intersections a b = [a, b]
