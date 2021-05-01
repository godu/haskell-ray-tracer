module RayTracer.Data.World
  ( World (objects, light),
    world,
  )
where

import RayTracer.Data.Light (Light)
import RayTracer.Data.Sphere (Sphere)
import Prelude (Eq, Maybe (Nothing), Show)

data World a = World
  { objects :: [Sphere a],
    light :: Maybe (Light a)
  }
  deriving (Show, Eq)

world = World [] Nothing
