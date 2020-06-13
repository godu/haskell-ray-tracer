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
                                                , RealFrac
                                                , (<$>)
                                                , ($)
                                                , (.)
                                                , (+)
                                                , (-)
                                                , (*)
                                                , (==)
                                                , (&&)
                                                , (<=)
                                                , abs
                                                , show
                                                , floor
                                                , max
                                                , min
                                                , unwords
                                                )
import           RayTracer.Data.Extra           ( (~=) )

data Color a = Color { red :: a
                     , green :: a
                     , blue :: a
                     }

color :: a -> a -> a -> Color a
color = Color

instance RealFrac a => Show (Color a) where
    show (Color r g b) =
        unwords $ (show . max 0 . min 255 . floor . (*) 256) <$> [r, g, b]

instance (Ord a, Fractional a) => Eq (Color a )where
    (Color a b c) == (Color a' b' c') = (a ~= a') && (b ~= b') && (c ~= c')

instance Num a => Num (Color a) where
    (Color a b c) + (Color a' b' c') = Color (a + a') (b + b') (c + c')
    (Color a b c) - (Color a' b' c') = Color (a - a') (b - b') (c - c')
    (Color a b c) * (Color a' b' c') = Color (a * a') (b * b') (c * c')

(*^) :: (Num a) => Color a -> a -> Color a
(Color a b c) *^ x = color (a * x) (b * x) (c * x)
