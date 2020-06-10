module RayTracer.Data.Canvas
    ( canvas
    , width
    , height
    , pixels
    , at
    , replace
    )
where

import           Data.Map.Strict                ( Map
                                                , empty
                                                , (!)
                                                , insert
                                                )
import           RayTracer.Data.Color           ( Color
                                                , color
                                                )

data Canvas a = Canvas { width :: Int
                       , height :: Int
                       , pixels :: Map (Int, Int) (Color a)
                       }

canvas :: Int -> Int -> Canvas a
canvas w h = Canvas w h empty

at :: Canvas a -> (Int, Int) -> Color a
at (Canvas _ _ pixels) i = pixels ! i

replace :: (Int, Int) -> Color a -> Canvas a -> Canvas a
replace i a (Canvas w h pixels) = Canvas w h $ insert i a pixels
