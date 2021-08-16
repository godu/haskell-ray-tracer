module RayTracer.Data.Canvas
  ( Canvas (),
    canvas,
    width,
    height,
    pixels,
    positions,
    at,
    replace,
    bulk,
  )
where

import Data.Ix (range)
import Data.Maybe (fromMaybe)
import Data.Vector
  ( Vector,
    replicate,
    (!?),
    (//),
  )
import RayTracer.Data.Color (Color, black, color)
import Prelude hiding (replicate)

data Canvas a = Canvas
  { width :: !Int,
    height :: !Int,
    pixels :: !(Vector (Color a))
  }
  deriving (Eq)

canvas :: Num a => (Int, Int) -> Canvas a
canvas (w, h) = Canvas w h $ replicate (w * h) black

at :: Num a => Canvas a -> (Int, Int) -> Color a
at (Canvas w _ pixels) (x, y) = fromMaybe (color 0 0 0) $ pixels !? (x + y * w)

replace :: (Int, Int) -> Color a -> Canvas a -> Canvas a
replace i a c = bulk c [(i, a)]

bulk :: (Functor t, Foldable t) => Canvas a -> t ((Int, Int), Color a) -> Canvas a
bulk (Canvas w h pixels) ps = Canvas w h nextPixels
  where
    toList = foldr (:) []
    ops = (\((x, y), c) -> (x + y * w, c)) <$> ps
    nextPixels = pixels // toList ops

positions :: Canvas a -> [[(Int, Int)]]
positions (Canvas w h _) =
  (\a -> (,a) <$> range (0, w - 1)) <$> range (0, h - 1)

instance (RealFrac a, Show a) => Show (Canvas a) where
  show c = "P3\n" <> show w <> " " <> show h <> "\n255\n" <> pixels <> "\n"
    where
      Canvas w h _ = c
      pixels =
        unlines $
          concat $
            (joinUntil " " ((<= 70) . length) <$> concatMap words)
              . fmap (show . at c)
              <$> positions c

joinUntil :: String -> (String -> Bool) -> [String] -> [String]
joinUntil break cond (x : xs) = joinUntil_ break cond xs x
  where
    joinUntil_ _ _ [] acc = [acc]
    joinUntil_ break cond (x : xs) acc
      | cond (acc <> break <> x) = joinUntil_ break cond xs (acc <> break <> x)
      | otherwise = acc : joinUntil_ break cond xs x
joinUntil _ _ [] = []
