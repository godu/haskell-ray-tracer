module RayTracer.Data.Canvas
    ( canvas
    , width
    , height
    , pixels
    , positions
    , at
    , replace
    )
where

import           Prelude                        ( Show
                                                , String
                                                , RealFrac
                                                , Int
                                                , Num
                                                , Bool
                                                , (<>)
                                                , (<$>)
                                                , ($)
                                                , (*)
                                                , (+)
                                                , (-)
                                                , (.)
                                                , (<=)
                                                , show
                                                , fmap
                                                , otherwise
                                                , length
                                                , words
                                                )
import           Data.Foldable                  ( concat
                                                , concatMap
                                                )
import           Data.Ix                        ( range )
import           Data.Map.Strict                ( Map
                                                , empty
                                                , (!?)
                                                , insert
                                                )
import           Data.List.Utils                ( join )
import           Data.Maybe                     ( fromMaybe )
import           RayTracer.Data.Color           ( Color
                                                , color
                                                )

data Canvas a = Canvas { width :: Int
                       , height :: Int
                       , pixels :: Map (Int, Int) (Color a)
                       }

canvas :: (Int, Int) -> Canvas a
canvas (w, h) = Canvas w h empty

at :: Num a => Canvas a -> (Int, Int) -> Color a
at (Canvas _ _ pixels) i = fromMaybe (color 0 0 0) $ pixels !? i

replace :: (Int, Int) -> Color a -> Canvas a -> Canvas a
replace i a (Canvas w h pixels) = Canvas w h $ insert i a pixels

positions (Canvas w h _) =
    (\a -> ((\b -> (b, a)) <$> range (0, w - 1))) <$> range (0, (h - 1))

instance (Num a, RealFrac a) => Show (Canvas a) where
    show c = "P3\n" <> show w <> " " <> show h <> "\n255\n" <> pixels <> "\n"
      where
        Canvas w h ps = c
        pixels =
            join "\n"
                $   concat
                $   (joinUntil " " ((<= 70) . length))
                <$> concatMap (words)
                <$> fmap (show . at c)
                <$> positions c

joinUntil :: String -> (String -> Bool) -> [String] -> [String]
joinUntil break cond (x : xs) = joinUntil_ break cond xs x
  where
    joinUntil_ break cond [] acc = [acc]
    joinUntil_ break cond (x : xs) acc
        | cond (acc <> break <> x) = joinUntil_ break
                                                cond
                                                xs
                                                (acc <> break <> x)
        | otherwise = acc : joinUntil_ break cond xs x
joinUntil _ _ [] = []
