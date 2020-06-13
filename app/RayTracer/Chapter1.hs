module RayTracer.Chapter1
  ( main
  )
where

import           Prelude                        ( IO
                                                , ($)
                                                , print
                                                )
import           RayTracer.Projectile           ( Projectile(Projectile)
                                                , Environment(Environment)
                                                , fire
                                                )
import           RayTracer.Data.Tuple           ( (*^)
                                                , vector
                                                , point
                                                , normalize
                                                )
import           Data.Foldable                  ( traverse_ )

main :: IO ()
main = do
  print "Fire projectile"
  traverse_ print $ fire environment projectile
  print "Fire optimal projectile"
  traverse_ print $ fire environment secondProjectile
 where
  gravity          = vector 0 (-0.1) 0
  wind             = vector (-0.1) 0 0
  environment      = Environment gravity wind

  position         = point 0 1 0
  velocity         = normalize $ vector 1 1 0
  projectile       = Projectile position velocity
  secondProjectile = Projectile position (velocity *^ 2)
