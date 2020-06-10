module RayTracer.Data.Extra
    ( (~=)
    )
where

import           Prelude                        ( Fractional
                                                , Eq
                                                , Ord
                                                , Show
                                                , Bool
                                                , ($)
                                                , (-)
                                                , (/)
                                                , (>=)
                                                , abs
                                                , fromInteger
                                                )

(~=) :: (Ord a, Fractional a) => a -> a -> Bool
a ~= b = (>=) precision $ abs $ a - b where precision = 1 / 1000
