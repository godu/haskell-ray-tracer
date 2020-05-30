{-# LANGUAGE RankNTypes #-}

module RayTracer.Data.Tuple where

import           Prelude

type Tuple a = Num a => (a, a, a, a)

x :: Num a => Tuple a -> a
x (a, _, _, _) = a

y :: Num a => Tuple a -> a
y (_, b, _, _) = b

z :: Num a => Tuple a -> a
z (_, _, c, _) = c

w :: Num a => Tuple a -> a
w (_, _, _, d) = d

tuple :: Num a => a -> a -> a -> a -> Tuple a
tuple a b c d = (a, b, c, d)

vector :: Num a => a -> a -> a -> Tuple a
vector a b c = tuple a b c 0

point :: Num a => a -> a -> a -> Tuple a
point a b c = tuple a b c 1

isVector :: Num a => Eq a => Tuple a -> Bool
isVector = (== 0) . w

isPoint :: Num a => Eq a => Tuple a -> Bool
isPoint = (== 1) . w
