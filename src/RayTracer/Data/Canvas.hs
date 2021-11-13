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
    imap,
  )
where

import Control.DeepSeq (force)
import Control.Parallel.Strategies (parListChunk, rpar, using)
import Data.Ix (range)
import qualified Data.List.Index as LI (imap)
import Data.List.Split (chunksOf)
import Data.Tuple (swap)
import GHC.Conc (numCapabilities)
import RayTracer.Data.Color (Color, black)

data Canvas a = Canvas
  { width :: !Int,
    height :: !Int,
    pixels :: [Color a]
  }
  deriving (Eq)

canvas :: Num a => (Int, Int) -> Canvas a
canvas (w, h) = Canvas w h $ replicate (w * h) black

at :: Canvas a -> (Int, Int) -> Color a
at (Canvas w _ pixels) (x, y) = pixels !! (x + y * w)

replace :: (Int, Int) -> Color a -> Canvas a -> Canvas a
replace (x, y) a c = c {pixels = xs ++ (a : tail ys)}
  where
    w = width c
    (xs, ys) = splitAt (x + y * w) $ pixels c

bulk :: (Foldable t) => Canvas a -> t ((Int, Int), Color a) -> Canvas a
bulk = foldr (uncurry replace)

positions :: Canvas a -> [[(Int, Int)]]
positions (Canvas w h _) =
  (\a -> (,a) <$> range (0, w - 1)) <$> range (0, h - 1)

instance (RealFrac a, Show a) => Show (Canvas a) where
  show c@(Canvas w h _) = "P3\n" <> show w <> " " <> show h <> "\n255\n" <> showPixels c <> "\n"
    where
      showPixels =
        unlines
          . concat
          . (`using` strategy)
          . map
            ( force . joinUntil " " ((<= 70) . length)
                . concatMap words
                . fmap show
            )
          . chunksOf w
          . pixels
      strategy = parListChunk chunkLength rpar
        where
          chunkLength = floor $ fromIntegral h / fromIntegral numCapabilities

joinUntil :: [a] -> ([a] -> Bool) -> [[a]] -> [[a]]
joinUntil break cond (x : xs) = go break cond xs x
  where
    go _ _ [] acc = [acc]
    go break cond (x : xs) acc
      | cond (acc <> break <> x) = go break cond xs (acc <> break <> x)
      | otherwise = acc : go break cond xs x
joinUntil _ _ [] = []

imap :: ((Int, Int) -> Color a -> Color a) -> Canvas a -> Canvas a
imap f (Canvas w h pixels) = Canvas w h (LI.imap (f . swap . (`quotRem` w)) pixels)
