{-# LANGUAGE RankNTypes #-}

module RayTracer.Data.Tuple where

import           Prelude

data Tuple a = Tuple a a a a deriving (Eq, Show)

instance (Num a) => Num (Tuple a) where
  (Tuple a b c d) + (Tuple a' b' c' d') =
    Tuple (a + a') (b + b') (c + c') (d + d')
  negate (Tuple a b c d) = Tuple (-a) (-b) (-c) (-d)


x :: Num a => Tuple a -> a
x (Tuple a _ _ _) = a

y :: Num a => Tuple a -> a
y (Tuple _ b _ _) = b

z :: Num a => Tuple a -> a
z (Tuple _ _ c _) = c

w :: Num a => Tuple a -> a
w (Tuple _ _ _ d) = d

tuple :: Num a => a -> a -> a -> a -> Tuple a
tuple = Tuple

vector :: Num a => a -> a -> a -> Tuple a
vector a b c = tuple a b c 0

point :: Num a => a -> a -> a -> Tuple a
point a b c = tuple a b c 1

isVector :: Num a => Eq a => Tuple a -> Bool
isVector = (== 0) . w

isPoint :: Num a => Eq a => Tuple a -> Bool
isPoint = (== 1) . w
