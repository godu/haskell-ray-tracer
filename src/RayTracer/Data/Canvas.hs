{-# LANGUAGE TupleSections #-}

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

import Data.Foldable
  ( Foldable (foldr),
    concat,
    concatMap,
  )
import Data.Ix (range)
import Data.Map.Strict
  ( Map,
    empty,
    insert,
    (!?),
  )
import Data.Maybe (fromMaybe)
import RayTracer.Data.Color
  ( Color,
    color,
  )
import Prelude
  ( Bool,
    Eq,
    Int,
    Num,
    RealFrac,
    Show,
    String,
    fmap,
    length,
    otherwise,
    show,
    uncurry,
    unlines,
    words,
    ($),
    (*),
    (+),
    (-),
    (.),
    (<$>),
    (<=),
    (<>),
  )

data Canvas a = Canvas
  { width :: !Int,
    height :: !Int,
    pixels :: !(Map (Int, Int) (Color a))
  }
  deriving (Eq)

canvas :: (Int, Int) -> Canvas a
canvas (w, h) = Canvas w h empty

at :: Num a => Canvas a -> (Int, Int) -> Color a
at (Canvas _ _ pixels) i = fromMaybe (color 0 0 0) $ pixels !? i

replace :: (Int, Int) -> Color a -> Canvas a -> Canvas a
replace i a (Canvas w h pixels) = Canvas w h $ insert i a pixels

bulk :: Foldable t => Canvas a -> t ((Int, Int), Color a) -> Canvas a
bulk = foldr (uncurry replace)

positions (Canvas w h _) =
  (\a -> (,a) <$> range (0, w - 1)) <$> range (0, h - 1)

instance (Num a, RealFrac a) => Show (Canvas a) where
  show c = "P3\n" <> show w <> " " <> show h <> "\n255\n" <> pixels <> "\n"
    where
      Canvas w h ps = c
      pixels =
        unlines $
          concat $
            (joinUntil " " ((<= 70) . length) <$> concatMap words)
              . fmap (show . at c)
              <$> positions c

joinUntil :: String -> (String -> Bool) -> [String] -> [String]
joinUntil break cond (x : xs) = joinUntil_ break cond xs x
  where
    joinUntil_ break cond [] acc = [acc]
    joinUntil_ break cond (x : xs) acc
      | cond (acc <> break <> x) = joinUntil_ break cond xs (acc <> break <> x)
      | otherwise = acc : joinUntil_ break cond xs x
joinUntil _ _ [] = []
