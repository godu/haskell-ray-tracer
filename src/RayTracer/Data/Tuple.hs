{-# LANGUAGE RankNTypes #-}

module RayTracer.Data.Tuple where

import           Prelude

data Tuple a = Tuple a a a a deriving (Eq, Show)

instance (Num a) => Num (Tuple a) where
  (Tuple a b c d) + (Tuple a' b' c' d') =
    Tuple (a + a') (b + b') (c + c') (d + d')
  negate (Tuple a b c d) = Tuple (-a) (-b) (-c) (-d)
  fromInteger i = Tuple a a a a where a = fromInteger i

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

zero :: Num a => Tuple a
zero = 0

(*^) :: Num a => Tuple a -> a -> Tuple a
(Tuple a b c d) *^ x = tuple (a * x) (b * x) (c * x) (d * x)

(/^) :: Fractional a => Tuple a -> a -> Tuple a
a /^ x = a *^ (1 / x)

magnitude :: Floating a => Tuple a -> a
magnitude (Tuple a b c d) = sqrt (a * a + b * b + c * c + d * d)

normalize :: (Fractional a, Floating a) => Tuple a -> Tuple a
normalize v = tuple (a / magnitude v)
                    (b / magnitude v)
                    (c / magnitude v)
                    (d / magnitude v)
  where (Tuple a b c d) = v

(.^) :: Num a => Tuple a -> Tuple a -> a
(Tuple a b c d) .^ (Tuple a' b' c' d') = a * a' + b * b' + c * c' + d * d'
