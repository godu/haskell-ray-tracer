module RayTracer.Data.Sphere
  ( Sphere(origin)
  , sphere
  , intersect
  )
where

import           Prelude                        ( Show
                                                , Num
                                                , Ord
                                                , Eq
                                                , Floating
                                                , (+)
                                                , (-)
                                                , (*)
                                                , (/)
                                                , (<)
                                                , sqrt
                                                , otherwise
                                                )
import           RayTracer.Data.Tuple           ( Tuple
                                                , point
                                                , (.^)
                                                )
import           RayTracer.Data.Ray             ( Ray(Ray) )
import           RayTracer.Data.Intersection    ( Intersection
                                                , intersection
                                                )

newtype Sphere a = Sphere { origin :: Tuple a } deriving (Show, Eq)

sphere :: Num a => Sphere a
sphere = Sphere (point 0 0 0)

intersect :: (Ord a, Floating a) => Ray a -> Sphere a -> [Intersection a Sphere]
intersect r s | discriminant < 0 = []
              | otherwise        = [intersection t1 s, intersection t2 s]
 where
  Ray o d      = r
  sphereToRay  = o - point 0 0 0
  a            = d .^ d
  b            = 2 * d .^ sphereToRay
  c            = sphereToRay .^ sphereToRay - 1
  discriminant = b * b - 4 * a * c
  t1           = ((-b) - sqrt discriminant) / (2 * a)
  t2           = ((-b) + sqrt discriminant) / (2 * a)
