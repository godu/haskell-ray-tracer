module RayTracer.Data.Sphere
  ( Sphere(origin, transformation)
  , sphere
  , intersect
  , setTranformation
  )
where

import           Prelude                        ( Show
                                                , Num
                                                , Ord
                                                , Eq
                                                , Floating
                                                , Fractional
                                                , ($)
                                                , (<$>)
                                                , (+)
                                                , (-)
                                                , (*)
                                                , (/)
                                                , (<)
                                                , fmap
                                                , sqrt
                                                , otherwise
                                                )
import           Data.Maybe                     ( Maybe
                                                , maybe
                                                )
import           RayTracer.Data.Matrix          ( Matrix
                                                , inverse
                                                )
import           RayTracer.Data.Tuple           ( Tuple
                                                , point
                                                , (.^)
                                                )
import           RayTracer.Data.Ray             ( Ray(Ray)
                                                , transform
                                                )
import           RayTracer.Data.Intersection    ( Intersection
                                                , intersection
                                                , intersections
                                                )
import           RayTracer.Transformation       ( identity )

data Sphere a = Sphere { origin :: Tuple a, transformation :: Matrix a } deriving (Show, Eq)

sphere :: Num a => Sphere a
sphere = Sphere (point 0 0 0) identity

intersect
  :: (Num a, Floating a, Eq a, Ord a, Fractional a)
  => Ray a
  -> Sphere a
  -> [Intersection a Sphere]
intersect r s = maybe [] (`intersect_` s) r2
 where
  r2 = fmap (`transform` r) $ inverse $ transformation s
  intersect_ r s
    | discriminant < 0 = intersections []
    | otherwise        = intersections [intersection t1 s, intersection t2 s]
   where
    Ray o d      = r
    sphereToRay  = o - point 0 0 0
    a            = d .^ d
    b            = 2 * d .^ sphereToRay
    c            = sphereToRay .^ sphereToRay - 1
    discriminant = b * b - 4 * a * c
    t1           = ((-b) - sqrt discriminant) / (2 * a)
    t2           = ((-b) + sqrt discriminant) / (2 * a)

setTranformation :: Matrix a -> Sphere a -> Sphere a
setTranformation t s = s { transformation = t }
