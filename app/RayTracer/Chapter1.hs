module RayTracer.Chapter1 where

import           Prelude                        ( IO
                                                , Show
                                                , Eq
                                                , Num
                                                , (+)
                                                , ($)
                                                , (.)
                                                , (<)
                                                , print
                                                , pure
                                                )
import           RayTracer.Data.Tuple           ( Tuple(..)
                                                , vector
                                                , point
                                                , normalize
                                                , y
                                                )
import           Data.List                      ( unfoldr
                                                , takeWhile
                                                )
import           Data.Foldable                  ( traverse_ )

data Projectile a = Projectile { position :: Tuple a
                               , velocity :: Tuple a
                               } deriving (Show)

data Environment a = Environment { gravity :: Tuple a
                                 , wind :: Tuple a
                                 } deriving (Show)

tick :: (Eq a, Num a) => Environment a -> Projectile a -> Projectile a
tick (Environment gravity wind) (Projectile position velocity) = Projectile
  nextPosition
  nextVelocity
 where
  nextPosition = position + velocity
  nextVelocity = velocity + gravity + wind

main :: IO ()
main = do
  print "Fire projectile"
  traverse_ print $ fireProjectile projectile
  print "Fire optimal projectile"
  traverse_ print $ fireProjectile secondProjectile
 where
  environment = Environment gravity wind
   where
    gravity = vector 0 (-0.1) 0
    wind    = vector (-0.1) 0 0

  fireProjectile = takeWhile aboveFloor . unfoldr
    (\projectile ->
      let nextProjectile = tick environment projectile
      in  pure (projectile, nextProjectile)
    )
    where aboveFloor (Projectile position _) = 0 < y position

  position   = point 0 1 0
  projectile = Projectile position velocity
    where velocity = normalize $ vector 1 1 0
  secondProjectile = Projectile position velocity
    where velocity = normalize $ vector 1 0.5 0
