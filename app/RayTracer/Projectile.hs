module RayTracer.Projectile
  ( Projectile(Projectile)
  , Environment(Environment)
  , fire
  , updateCanvas
  , fuchsia
  )
where

import           Prelude                        ( Ord
                                                , Num
                                                , Fractional
                                                , Bool
                                                , Show
                                                , Int
                                                , (.)
                                                , (<)
                                                , (+)
                                                , pure
                                                )
import           RayTracer.Data.Tuple           ( Tuple(y) )
import           RayTracer.Data.Color           ( Color
                                                , color
                                                )
import           RayTracer.Data.Canvas          ( Canvas
                                                , replace
                                                )
import           Data.List                      ( unfoldr
                                                , takeWhile
                                                )
import           Data.Foldable                  ( Foldable
                                                , foldr
                                                )

data Projectile a = Projectile { position :: Tuple a
                              , velocity :: Tuple a
                              } deriving (Show)

data Environment a = Environment { gravity :: Tuple a
                                , wind :: Tuple a
                                } deriving (Show)

tick :: Num a => Environment a -> Projectile a -> Projectile a
tick (Environment gravity wind) (Projectile position velocity) = Projectile
  nextPosition
  nextVelocity
 where
  nextPosition = position + velocity
  nextVelocity = velocity + gravity + wind

aboveFloor :: (Num a, Ord a) => Projectile a -> Bool
aboveFloor (Projectile position _) = 0 < y position

fire :: (Num a, Ord a) => Environment a -> Projectile a -> [Projectile a]
fire environment = takeWhile aboveFloor . unfoldr go
 where
  go projectile = pure (projectile, nextProjectile)
    where nextProjectile = tick environment projectile

updateCanvas :: Foldable t => Color a -> Canvas a -> t (Int, Int) -> Canvas a
updateCanvas color = foldr (go color) where go color pos = replace pos color

fuchsia :: Fractional a => Color a
fuchsia = color 1 0.2 1
