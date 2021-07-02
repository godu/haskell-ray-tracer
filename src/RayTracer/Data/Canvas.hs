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

import Data.Ix
import Data.Maybe
import qualified Data.Vector as V
import RayTracer.Data.Color

data Canvas a = Canvas
  { width :: !Int,
    height :: !Int,
    pixels :: !(V.Vector (Color a))
  }
  deriving (Eq)

canvas :: Num a => (Int, Int) -> Canvas a
canvas (w, h) = Canvas w h $ V.replicate (w * h) black

at :: Num a => Canvas a -> (Int, Int) -> Color a
at (Canvas w _ pixels) (x, y) = fromMaybe (color 0 0 0) $ pixels V.!? (x + y * w)

replace :: (Int, Int) -> Color a -> Canvas a -> Canvas a
replace i a c = bulk c [(i, a)]

bulk :: (Functor t, Foldable t) => Canvas a -> t ((Int, Int), Color a) -> Canvas a
bulk (Canvas w h pixels) ps = Canvas w h nextPixels
  where
    toList = foldr (:) []
    ops = (\((x, y), c) -> (x + y * w, c)) <$> ps
    nextPixels = pixels V.// toList ops

positions (Canvas w h _) =
  (\a -> (,a) <$> range (0, w - 1)) <$> range (0, h - 1)

instance (Num a, RealFrac a) => Show (Canvas a) where
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
