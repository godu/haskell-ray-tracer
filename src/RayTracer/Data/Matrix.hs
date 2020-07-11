module RayTracer.Data.Matrix
  ( Matrix
  , (*^)
  , fromList
  , at
  , update
  , one
  , transpose
  , determinant
  , submatrix
  , minor
  , cofactor
  , inverse
  )
where

import           Prelude                        ( Show
                                                , Eq
                                                , Num
                                                , Fractional
                                                , Int
                                                , Ord
                                                , Bool
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
                                                , (/)
                                                , not
                                                , take
                                                , quotRem
                                                , uncurry
                                                , sum
                                                , product
                                                , zip
                                                , show
                                                , foldr
                                                , fmap
                                                , min
                                                , negate
                                                , odd
                                                , otherwise
                                                , pure
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
import           RayTracer.Data.Extra           ( (~=) )
import qualified Data.Vector                   as V
                                                ( Vector
                                                , (!?)
                                                , update
                                                , fromList
                                                , toList
                                                , ifilter
                                                , imap
                                                , and
                                                , zipWith
                                                )

data Matrix a = Matrix { dimension :: (Int, Int)
                       , values :: V.Vector a
                       } deriving (Show)

instance (Ord a, Fractional a) => Eq (Matrix a) where
  (Matrix (w, h) as) == (Matrix (w', h') as') =
    (w == w') && (h == h') && (V.and $ V.zipWith (~=) as as')

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

update :: (Int, Int) -> a -> Matrix a -> Matrix a
update (x, y) a (Matrix (w, h) as) =
  Matrix (w, h) $ V.update as $ V.fromList [((x * h + y), a)]

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
determinant a = sum $ (go) <$> [0 .. (y - 1)]
 where
  (_, y) = dimension a
  go y = (fromMaybe 0 $ at a (0, y)) * cofactor 0 y a

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

inverse :: (Num a, Eq a, Fractional a) => Matrix a -> Maybe (Matrix a)
inverse a | determinant a == 0 = Nothing
          | otherwise          = pure $ Matrix (w, h) $ V.imap go as
 where
  Matrix (w, h) as = a
  d                = determinant a
  go i v = c / d
   where
    (x, y) = i `quotRem` h
    c      = cofactor y x a
