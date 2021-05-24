module RayTracer.Chapter1
  ( main,
  )
where

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
  ( Semigroup ((<>)),
    Show (show),
    String,
    ($),
    (<$>),
  )

main :: [String]
main =
  "Fire projectile" :
  (show <$> fire environment projectile)
    <> ["Fire optimal projectile"]
    <> (show <$> fire environment secondProjectile)
  where
    gravity = vector 0 (-0.1) 0
    wind = vector (-0.1) 0 0
    environment = Environment gravity wind

    position = point 0 1 0
    velocity = normalize $ vector 1 1 0
    projectile = Projectile position velocity
    secondProjectile = Projectile position (velocity *^ 2)
