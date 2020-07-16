module RayTracer.Data.Light
    ( Light(position, intensity)
    , pointLight
    )
where

import           Prelude                        ( )
import           RayTracer.Data.Tuple           ( Tuple )
import           RayTracer.Data.Color           ( Color )

data Light a = Light {position :: Tuple a, intensity :: Color a}

pointLight :: Tuple a -> Color a -> Light a
pointLight = Light
