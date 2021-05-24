module RayTracer.Data.Tuple
  ( Tuple (Tuple, x, y, z, w),
    tuple,
    vector,
    point,
    isVector,
    isPoint,
    zero,
    (*^),
    (/^),
    magnitude,
    normalize,
    (.^),
    reflect,
  )
where

import RayTracer.Extra ((~=))
import Prelude
  ( Bool,
    Eq,
    Floating,
    Fractional,
    Num (abs, fromInteger, negate, signum, (*), (+), (-)),
    Ord,
    Show,
    sqrt,
    undefined,
    (&&),
    (.),
    (/),
    (==),
  )

data Tuple a = Tuple
  { x :: !a,
    y :: !a,
    z :: !a,
    w :: !a
  }
  deriving (Show)

instance (Ord a, Fractional a) => Eq (Tuple a) where
  (Tuple a b c d) == (Tuple a' b' c' d') =
    (a ~= a') && (b ~= b') && (c ~= c') && (d ~= d')

instance Num a => Num (Tuple a) where
  (Tuple a b c d) + (Tuple a' b' c' d') =
    Tuple (a + a') (b + b') (c + c') (d + d')

  negate (Tuple a b c d) = Tuple (- a) (- b) (- c) (- d)

  fromInteger i = Tuple a a a a where a = fromInteger i

  (Tuple a b c _) * (Tuple a' b' c' _) =
    vector (b * c' - c * b') (c * a' - a * c') (a * b' - b * a')

  abs = undefined
  signum = undefined

tuple :: a -> a -> a -> a -> Tuple a
tuple = Tuple

vector :: Num a => a -> a -> a -> Tuple a
vector a b c = tuple a b c 0

point :: Num a => a -> a -> a -> Tuple a
point a b c = tuple a b c 1

isVector :: Num a => Eq a => Tuple a -> Bool
isVector = (== 0) . w

isPoint :: Num a => Eq a => Tuple a -> Bool
isPoint = (== 1) . w

zero :: (Eq a, Num a) => Tuple a
zero = 0

(*^) :: Num a => Tuple a -> a -> Tuple a
(Tuple a b c d) *^ x = tuple (a * x) (b * x) (c * x) (d * x)

(/^) :: Fractional a => Tuple a -> a -> Tuple a
a /^ x = a *^ (1 / x)

magnitude :: Floating a => Tuple a -> a
magnitude (Tuple a b c d) = sqrt (a * a + b * b + c * c + d * d)

normalize :: (Fractional a, Floating a) => Tuple a -> Tuple a
normalize v =
  tuple
    (a / m)
    (b / m)
    (c / m)
    (d / m)
  where
    m = magnitude v
    (Tuple a b c d) = v

(.^) :: Num a => Tuple a -> Tuple a -> a
(Tuple a b c d) .^ (Tuple a' b' c' d') = a * a' + b * b' + c * c' + d * d'

reflect :: Num a => Tuple a -> Tuple a -> Tuple a
reflect in_ normal = in_ - normal *^ 2 *^ (in_ .^ normal)
