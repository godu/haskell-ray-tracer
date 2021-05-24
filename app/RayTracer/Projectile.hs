{-# LANGUAGE TupleSections #-}

module RayTracer.Projectile
  ( Projectile (Projectile),
    Environment (Environment),
    fire,
    updateCanvas,
    fuchsia,
  )
where

import Data.Foldable
  ( Foldable,
  )
import Data.List
  ( takeWhile,
    unfoldr,
  )
import RayTracer.Data.Canvas
  ( Canvas,
    bulk,
  )
import RayTracer.Data.Color
  ( Color,
    color,
  )
import RayTracer.Data.Tuple (Tuple (y))
import Prelude
  ( Bool,
    Fractional,
    Functor,
    Int,
    Num,
    Ord,
    Show,
    pure,
    ($),
    (+),
    (.),
    (<),
    (<$>),
  )

data Projectile a = Projectile
  { position :: !(Tuple a),
    velocity :: !(Tuple a)
  }
  deriving (Show)

data Environment a = Environment
  { gravity :: !(Tuple a),
    wind :: !(Tuple a)
  }
  deriving (Show)

tick :: Num a => Environment a -> Projectile a -> Projectile a
tick (Environment gravity wind) (Projectile position velocity) =
  Projectile
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
      where
        nextProjectile = tick environment projectile

updateCanvas :: (Functor t, Foldable t) => Color a -> Canvas a -> t (Int, Int) -> Canvas a
updateCanvas color canvas ps = bulk canvas $ (,color) <$> ps

fuchsia :: Fractional a => Color a
fuchsia = color 1 0.2 1
