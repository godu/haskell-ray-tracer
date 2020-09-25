module RayTracer.Chapter1
  ( main,
  )
where

import Data.Foldable (traverse_)
import RayTracer.Data.Tuple
  ( normalize,
    point,
    vector,
    (*^),
  )
import RayTracer.Projectile
  ( Environment (Environment),
    Projectile (Projectile),
    fire,
  )
import Prelude
  ( IO,
    print,
    ($),
  )

main :: IO ()
main = do
  print "Fire projectile"
  traverse_ print $ fire environment projectile
  print "Fire optimal projectile"
  traverse_ print $ fire environment secondProjectile
  where
    gravity = vector 0 (-0.1) 0
    wind = vector (-0.1) 0 0
    environment = Environment gravity wind

    position = point 0 1 0
    velocity = normalize $ vector 1 1 0
    projectile = Projectile position velocity
    secondProjectile = Projectile position (velocity *^ 2)
