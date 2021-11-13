module RayTracer.Data.Pattern.PerturbedPattern
  ( PerturbedPattern (PerturbedPattern, transformation, a, b),
    perturbedPattern,
  )
where

import Data.Bits (Bits, (.&.))
import RayTracer.Data.Matrix (Matrix)
import RayTracer.Data.Pattern as P (Pattern (..))
import RayTracer.Data.Pattern.Extra (patternAtPattern)
import RayTracer.Data.Tuple (Tuple (Tuple))
import RayTracer.Transformation (identity)

data PerturbedPattern p a = PerturbedPattern
  { transformation :: !(Matrix a),
    a :: !(p a),
    b :: !(p a)
  }
  deriving (Eq, Show)

perturbedPattern :: Num a => p a -> p a -> PerturbedPattern p a
perturbedPattern = PerturbedPattern identity

instance (RealFrac a, P.Pattern p a) => P.Pattern (PerturbedPattern p) a where
  getTransformation = transformation
  setTransformation p t = p {transformation = t}
  patternAt (PerturbedPattern _ a b) p = patternAtPattern (if noise p then a else b) p

fade :: Num a => a -> a
fade t = t * t * t * (t * (t * 6 - 15) + 10)

lerp :: Num a => a -> a -> a -> a
lerp t a b = a + t * (b - a)

grad :: (Bits h, Ord h, Num h, Num a) => h -> a -> a -> a -> a
grad hash x y z =
  (if (h .&. 1) == 0 then u else - u)
    + (if (h .&. 2) == 0 then v else - v)
  where
    h = hash .&. 15
    u = if h < 8 then x else y
    v
      | h < 4 = y
      | h == 12 || h == 14 = x
      | otherwise = z

permutation :: [Int]
permutation =
  cycle
    [ 151,
      160,
      137,
      91,
      90,
      15,
      131,
      13,
      201,
      95,
      96,
      53,
      194,
      233,
      7,
      225,
      140,
      36,
      103,
      30,
      69,
      142,
      8,
      99,
      37,
      240,
      21,
      10,
      23,
      190,
      6,
      148,
      247,
      120,
      234,
      75,
      0,
      26,
      197,
      62,
      94,
      252,
      219,
      203,
      117,
      35,
      11,
      32,
      57,
      177,
      33,
      88,
      237,
      149,
      56,
      87,
      174,
      20,
      125,
      136,
      171,
      168,
      68,
      175,
      74,
      165,
      71,
      134,
      139,
      48,
      27,
      166,
      77,
      146,
      158,
      231,
      83,
      111,
      229,
      122,
      60,
      211,
      133,
      230,
      220,
      105,
      92,
      41,
      55,
      46,
      245,
      40,
      244,
      102,
      143,
      54,
      65,
      25,
      63,
      161,
      1,
      216,
      80,
      73,
      209,
      76,
      132,
      187,
      208,
      89,
      18,
      169,
      200,
      196,
      135,
      130,
      116,
      188,
      159,
      86,
      164,
      100,
      109,
      198,
      173,
      186,
      3,
      64,
      52,
      217,
      226,
      250,
      124,
      123,
      5,
      202,
      38,
      147,
      118,
      126,
      255,
      82,
      85,
      212,
      207,
      206,
      59,
      227,
      47,
      16,
      58,
      17,
      182,
      189,
      28,
      42,
      223,
      183,
      170,
      213,
      119,
      248,
      152,
      2,
      44,
      154,
      163,
      70,
      221,
      153,
      101,
      155,
      167,
      43,
      172,
      9,
      129,
      22,
      39,
      253,
      19,
      98,
      108,
      110,
      79,
      113,
      224,
      232,
      178,
      185,
      112,
      104,
      218,
      246,
      97,
      228,
      251,
      34,
      242,
      193,
      238,
      210,
      144,
      12,
      191,
      179,
      162,
      241,
      81,
      51,
      145,
      235,
      249,
      14,
      239,
      107,
      49,
      192,
      214,
      31,
      181,
      199,
      106,
      157,
      184,
      84,
      204,
      176,
      115,
      121,
      50,
      45,
      127,
      4,
      150,
      254,
      138,
      236,
      205,
      93,
      222,
      114,
      67,
      29,
      24,
      72,
      243,
      141,
      128,
      195,
      78,
      66,
      215,
      61,
      156,
      180
    ]

noise :: (RealFrac a) => Tuple a -> Bool
noise (Tuple x y z _) =
  lerp
    w
    ( lerp
        v
        ( lerp
            u
            (grad (permutation !! aa) x'' y'' z'')
            (grad (permutation !! ba) (x'' -1) y'' z'')
        )
        ( lerp
            u
            (grad (permutation !! ab) x'' (y'' -1) z'')
            (grad (permutation !! bb) x'' (y'' -1) z'')
        )
    )
    ( lerp
        v
        ( lerp
            u
            (grad (permutation !! (aa + 1)) x'' y'' (z'' -1))
            (grad (permutation !! (ba + 1)) (x'' -1) y'' (z'' -1))
        )
        ( lerp
            u
            (grad (permutation !! (ab + 1)) x'' (y'' -1) (z'' -1))
            (grad (permutation !! (bb + 1)) x'' (y'' -1) (z'' -1))
        )
    )
    > 0
  where
    x', y', z' :: Int
    x' = floor x .&. 255
    y' = floor y .&. 255
    z' = floor z .&. 255

    x'' = x - fromIntegral (floor x)
    y'' = y - fromIntegral (floor y)
    z'' = z - fromIntegral (floor z)

    u = fade x''
    v = fade y''
    w = fade z''

    a = (permutation !! x') + y'
    aa = (permutation !! a) + z'
    ab = (permutation !! (a + 1)) + z'
    b = (permutation !! (x' + 1)) + y'
    ba = (permutation !! b) + z'
    bb = (permutation !! (b + 1)) + z'
