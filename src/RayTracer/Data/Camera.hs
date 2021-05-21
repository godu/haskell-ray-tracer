{-# LANGUAGE TupleSections #-}

module RayTracer.Data.Camera
  ( Camera
      ( hsize,
        vsize,
        fieldOfView,
        transformation,
        halfHeight,
        halfWidth,
        pixelSize
      ),
    camera,
    rayForPixel,
    render,
  )
where

import Control.Applicative (liftA2)
import Data.Maybe (mapMaybe)
import RayTracer.Data.Canvas (Canvas, canvas, replace)
import RayTracer.Data.Matrix (Matrix, inverse, (*^))
import RayTracer.Data.Ray (Ray, ray)
import RayTracer.Data.Shape (Shape)
import RayTracer.Data.Tuple (normalize, point, vector)
import RayTracer.Data.World (World, colorAt)
import RayTracer.Transformation (identity)
import Prelude
  ( Eq,
    Floating,
    Foldable (foldr),
    Fractional,
    Int,
    Maybe,
    Num ((*), (+), (-)),
    Ord ((>=)),
    RealFrac,
    Show,
    fromIntegral,
    maybe,
    quotRem,
    tan,
    ($),
    (/),
    (<$>),
    (<>),
  )

data Camera a = Camera
  { hsize :: !Int,
    vsize :: !Int,
    fieldOfView :: !a,
    transformation :: !(Matrix a),
    halfWidth :: !a,
    halfHeight :: !a,
    pixelSize :: !a
  }
  deriving (Show, Eq)

camera :: (Floating a, Ord a) => Int -> Int -> a -> Camera a
camera hsize vsize fieldOfView =
  Camera
    hsize
    vsize
    fieldOfView
    identity
    halfWidth
    halfHeight
    pixelSize
  where
    halfView = tan $ fieldOfView / 2
    aspect = fromIntegral hsize / fromIntegral vsize
    halfWidth = if aspect >= 1 then halfView else halfView * aspect
    halfHeight = if aspect >= 1 then halfView / aspect else halfView
    pixelSize = (halfWidth * 2) / fromIntegral hsize

rayForPixel :: (Eq a, Floating a) => Camera a -> (Int, Int) -> Maybe (Ray a)
rayForPixel c (px, py) = liftA2 ray origin direction
  where
    xOffset = (fromIntegral px + 0.5) * pixelSize c
    yOffset = (fromIntegral py + 0.5) * pixelSize c
    worldX = halfWidth c - xOffset
    worldY = halfHeight c - yOffset

    t = inverse $ transformation c
    pixel = (*^ point worldX worldY (-1)) <$> t
    origin = (*^ point 0 0 0) <$> t
    direction = normalize <$> liftA2 (-) pixel origin

render :: (Shape o a, Eq a, Floating a, RealFrac a, Eq (o a)) => Camera a -> World o a -> Canvas a
render camera world = image
  where
    pixels = (`quotRem` vsize camera) <$> [0 .. hsize camera * vsize camera - 1]
    image =
      foldr
        updateCanvas
        (canvas (hsize camera, vsize camera))
        $ mapMaybe
          (renderPixel canvas)
          pixels
    renderPixel canvas pixel = (pixel,) <$> color
      where
        ray = rayForPixel camera pixel
        color = colorAt world <$> ray
    updateCanvas (pixel, color) canvas = replace pixel color canvas
