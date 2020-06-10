module RayTracer.Data.Color
    ( Color(red, green, blue)
    , color
    , (*^)
    )
where

import           Prelude                        ( Num
                                                , Eq
                                                , Ord
                                                , Show
                                                , Fractional
                                                , ($)
                                                , (+)
                                                , (-)
                                                , (*)
                                                , (==)
                                                , (&&)
                                                , (<=)
                                                , abs
                                                )
import           RayTracer.Data.Extra           ( (~=) )

data Color a = Color { red :: a
                     , green :: a
                     , blue :: a
                     } deriving (Show)

color :: a -> a -> a -> Color a
color = Color

instance (Ord a, Fractional a) => Eq (Color a )where
    (Color a b c) == (Color a' b' c') = (a ~= a') && (b ~= b') && (c ~= c')

instance Num a => Num (Color a) where
    (Color a b c) + (Color a' b' c') = Color (a + a') (b + b') (c + c')
    (Color a b c) - (Color a' b' c') = Color (a - a') (b - b') (c - c')
    (Color a b c) * (Color a' b' c') = Color (a * a') (b * b') (c * c')


(*^) :: (Num a) => Color a -> a -> Color a
(Color a b c) *^ x = color (a * x) (b * x) (c * x)
