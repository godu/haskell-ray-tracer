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
import RayTracer.Data.Canvas (Canvas, canvas, imap)
import RayTracer.Data.Color (black)
import RayTracer.Data.Matrix (Matrix, inverse, (*^))
import RayTracer.Data.Ray (Ray, ray)
import RayTracer.Data.Shape (Shape)
import RayTracer.Data.Tuple (normalize, point)
import RayTracer.Data.World (World, colorAt, depth)
import RayTracer.Transformation (identity)

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

render :: (Shape o p a, Floating a, RealFrac a) => Camera a -> World (o p) a -> Canvas a
render camera world = imap renderPixel $ canvas (hsize camera, vsize camera)
  where
    renderPixel pixel _ = color
      where
        ray = rayForPixel camera pixel
        color = maybe black (colorAt depth world) ray
