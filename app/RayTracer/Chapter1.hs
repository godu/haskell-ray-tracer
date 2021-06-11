module RayTracer.Chapter1
  ( main,
  )
where

import qualified RayTracer.Data.Tuple as T
import RayTracer.Projectile

main :: [String]
main =
  "Fire projectile" :
  (show <$> fire environment projectile)
    <> ["Fire optimal projectile"]
    <> (show <$> fire environment secondProjectile)
  where
    gravity = T.vector 0 (-0.1) 0
    wind = T.vector (-0.1) 0 0
    environment = Environment gravity wind

    position = T.point 0 1 0
    velocity = T.normalize $ T.vector 1 1 0
    projectile = Projectile position velocity
    secondProjectile = Projectile position (velocity T.*^ 2)
