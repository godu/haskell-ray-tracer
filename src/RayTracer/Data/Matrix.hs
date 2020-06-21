module RayTracer.Data.Matrix
  ( Matrix
  , (*^)
  , fromList
  , at
  , one
  )
where

import           Prelude                        ( Show
                                                , Eq
                                                , Num
                                                , Int
                                                , Maybe
                                                , String
                                                , ($)
                                                , (<$>)
                                                , (<>)
                                                , (+)
                                                , (-)
                                                , (.)
                                                , (*)
                                                , (==)
                                                , (&&)
                                                , take
                                                , quotRem
                                                , uncurry
                                                , sum
                                                , product
                                                , zip
                                                , show
                                                , fmap
                                                , min
                                                )
import           Data.List                      ( concat
                                                , replicate
                                                )
import           Data.Maybe                     ( Maybe(Nothing)
                                                , catMaybes
                                                , fromMaybe
                                                )
import           Data.Tuple                     ( swap )
import           RayTracer.Data.Tuple           ( Tuple(x, y, z, w)
                                                , tuple
                                                )
import qualified Data.Vector                   as V
                                                ( Vector
                                                , (!?)
                                                , fromList
                                                )

data Matrix a = Matrix { dimension :: (Int, Int)
                       , values :: V.Vector a
                       } deriving (Show, Eq)

instance Num a => Num (Matrix a) where
  a * b = Matrix (w'', h'') $ V.fromList $ compute a b <$> cells
   where
    Matrix (w , h ) as = a
    Matrix (w', h') bs = b
    w''                = min w w'
    h''                = min h h'

    cells              = (`quotRem` h'') <$> [0 .. (w'' * h'' - 1)]

    toList (a, b) = [a, b]
    compute :: Num a => Matrix a -> Matrix a -> (Int, Int) -> a
    compute a b (x, y) =
      sum $ product . toList <$> zip (a `rowAt` x) (b `columnAt` y)

    rowAt :: Matrix a -> Int -> [a]
    (Matrix (w, h) xs) `rowAt` i =
      catMaybes $ ((V.!?) xs . (+) (h * i)) <$> [0 .. (h - 1)]
    columnAt :: Matrix a -> Int -> [a]
    (Matrix (_, h) xs) `columnAt` i =
      catMaybes $ ((V.!?) xs . (+) i . (*) h) <$> [0 .. (w - 1)]


fromList :: Int -> Int -> [a] -> Matrix a
fromList x y as = Matrix (x, y) $ V.fromList $ take (x * y) as

at :: Matrix a -> (Int, Int) -> Maybe a
at (Matrix (_, height) as) (x, y) = (V.!?) as (x * height + y)

(*^) :: Num a => Matrix a -> Tuple a -> Tuple a
m *^ t = toTuple $ (m *) $ toMatrix t
 where
  toMatrix t = fromList 4 1 [x t, y t, z t, w t]
  toTuple m = tuple (fromMaybe 0 $ m `at` (0, 0))
                    (fromMaybe 0 $ m `at` (1, 0))
                    (fromMaybe 0 $ m `at` (2, 0))
                    (fromMaybe 0 $ m `at` (3, 0))

one :: Num a => Int -> Matrix a
one x =
  fromList x x
    $   concat
    $   (\i -> replicate i 0 <> [1] <> replicate (x - i - 1) 0)
    <$> [0 .. x]
