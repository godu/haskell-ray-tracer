module RayTracer.Data.Matrix
  ( Matrix
  , fromList
  , at
  )
where

import           Prelude                        ( Show
                                                , Eq
                                                , Int
                                                , Maybe
                                                , ($)
                                                , (+)
                                                , (*)
                                                , (==)
                                                , (&&)
                                                , take
                                                )
import           Data.Maybe                     ( Maybe(Nothing) )
import qualified Data.Vector                   as V
                                                ( Vector
                                                , (!?)
                                                , fromList
                                                )

data Matrix a = Matrix { dimension :: (Int, Int)
                       , values :: V.Vector a
                       } deriving (Show, Eq)

fromList :: Int -> Int -> [a] -> Matrix a
fromList x y as = Matrix (x, y) $ V.fromList $ take (x * y) as

at :: Matrix a -> (Int, Int) -> Maybe a
at (Matrix (width, _) as) (x, y) = (V.!?) as (x * width + y)
