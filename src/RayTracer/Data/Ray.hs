module RayTracer.Data.Ray
  ( Ray(Ray, origin, direction)
  , ray
  , position
  )
where

import           Prelude                        ( Num
                                                , Eq
                                                , Show
                                                , (+)
                                                , (*)
                                                )
import           RayTracer.Data.Tuple           ( Tuple
                                                , (*^)
                                                )
import           RayTracer.Data.Extra           ( (~=) )

data Ray a = Ray { origin :: Tuple a, direction :: Tuple a } deriving (Show)

-- instance (Ord a, Fractional a) => Eq (Tuple a) where
--   (Tuple a b c d) == (Tuple a' b' c' d') =
--     (a ~= a') && (b ~= b') && (c ~= c') && (d ~= d')

ray :: Tuple a -> Tuple a -> Ray a
ray = Ray

position :: Num a => a -> Ray a -> Tuple a
position t (Ray o d) = o + (d *^ t)
