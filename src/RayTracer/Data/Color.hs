module RayTracer.Data.Color
  ( Color (red, green, blue),
    color,
    white,
    black,
    (*^),
  )
where

import RayTracer.Extra ((~=))

data Color a = Color
  { red :: !a,
    green :: !a,
    blue :: !a
  }

color :: a -> a -> a -> Color a
color = Color

instance (RealFrac a) => Show (Color a) where
  show (Color r g b) =
    unwords $
      show
        . max 0
        . min 255
        . floor
        . (*) 256
        <$> [r, g, b]

instance (Ord a, Fractional a) => Eq (Color a) where
  (Color a b c) == (Color a' b' c') = (a ~= a') && (b ~= b') && (c ~= c')

instance Num a => Num (Color a) where
  (Color a b c) + (Color a' b' c') = Color (a + a') (b + b') (c + c')
  (Color a b c) - (Color a' b' c') = Color (a - a') (b - b') (c - c')
  (Color a b c) * (Color a' b' c') = Color (a * a') (b * b') (c * c')
  fromInteger a = Color a' a' a'
    where
      a' = fromInteger a
  abs = undefined
  signum = undefined

instance Functor Color where
  fmap f (Color a b c) = Color (f a) (f b) (f c)

(*^) :: (Num a) => Color a -> a -> Color a
(Color a b c) *^ x = color (a * x) (b * x) (c * x)

black :: Num a => Color a
black = color 0 0 0

white :: Num a => Color a
white = color 1 1 1
