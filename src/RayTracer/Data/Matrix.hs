module RayTracer.Data.Matrix
  ( Matrix
  , (*^)
  , fromList
  , at
  , one
  , transpose
  , determinant
  , submatrix
  , minor
  , cofactor
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
                                                , (||)
                                                , not
                                                , take
                                                , quotRem
                                                , uncurry
                                                , sum
                                                , product
                                                , zip
                                                , show
                                                , fmap
                                                , min
                                                , negate
                                                , odd
                                                , otherwise
                                                )
import           Data.List                      ( concatMap
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
                                                , toList
                                                , ifilter
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
    $ concatMap (\i -> replicate i 0 <> [1] <> replicate (x - i - 1) 0)
    $ [0 .. x]

transpose :: Matrix a -> Matrix a
transpose (Matrix (w, h) as) = Matrix (w, h) bs
 where
  is = concatMap (\i -> (\j -> j * w + i) <$> [0 .. w - 1]) $ [0 .. h - 1]
  bs = V.fromList $ catMaybes $ (as V.!?) <$> is


determinant :: Num a => Matrix a -> a
determinant (Matrix (2, 2) as) = (a * d) - (b * c)
  where [a, b, c, d] = V.toList as

submatrix :: Int -> Int -> Matrix a -> Matrix a
submatrix x y (Matrix (w, h) as) = Matrix (w - 1, h - 1)
  $ V.ifilter (\i _ -> not (belongsTo (w, h) (x, y) i)) as
 where
  belongsTo (w, h) (x, y) i = x' == x || y' == y
    where (x', y') = i `quotRem` h

minor :: Num a => Int -> Int -> Matrix a -> a
minor x y = determinant . submatrix x y

cofactor :: Num a => Int -> Int -> Matrix a -> a
cofactor x y | odd $ x + y = negate . minor x y
             | otherwise   = minor x y
