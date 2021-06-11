{-# LANGUAGE FlexibleContexts #-}
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

import Control.Applicative
import Data.Maybe
import qualified RayTracer.Data.Canvas as C
import qualified RayTracer.Data.Matrix as M
import qualified RayTracer.Data.Pattern as P
import qualified RayTracer.Data.Ray as R
import qualified RayTracer.Data.Shape as S
import qualified RayTracer.Data.Tuple as T
import qualified RayTracer.Data.World as W
import RayTracer.Transformation

data Camera a = Camera
  { hsize :: !Int,
    vsize :: !Int,
    fieldOfView :: !a,
    transformation :: !(M.Matrix a),
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

rayForPixel :: (Eq a, Floating a) => Camera a -> (Int, Int) -> Maybe (R.Ray a)
rayForPixel c (px, py) = liftA2 R.ray origin direction
  where
    xOffset = (fromIntegral px + 0.5) * pixelSize c
    yOffset = (fromIntegral py + 0.5) * pixelSize c
    worldX = halfWidth c - xOffset
    worldY = halfHeight c - yOffset

    t = M.inverse $ transformation c
    pixel = (M.*^ T.point worldX worldY (-1)) <$> t
    origin = (M.*^ T.point 0 0 0) <$> t
    direction = T.normalize <$> liftA2 (-) pixel origin

render :: (S.Shape o p a, Eq a, Floating a, RealFrac a, Eq (o p a), P.Pattern p a) => Camera a -> W.World (o p) a -> C.Canvas a
render camera world = image
  where
    pixels = (`quotRem` vsize camera) <$> [0 .. hsize camera * vsize camera - 1]
    image =
      C.bulk
        (C.canvas (hsize camera, vsize camera))
        $ mapMaybe
          (renderPixel C.canvas)
          pixels
    renderPixel _ pixel = (pixel,) <$> color
      where
        ray = rayForPixel camera pixel
        color = W.colorAt world <$> ray
