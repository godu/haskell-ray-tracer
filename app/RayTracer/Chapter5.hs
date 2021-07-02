module RayTracer.Chapter5
  ( main,
    Pattern,
    colorPattern,
    stripePattern,
  )
where

import Data.Maybe
import qualified RayTracer.Data.Canvas as C
import qualified RayTracer.Data.Color as C
import qualified RayTracer.Data.Intersection as I
import qualified RayTracer.Data.Material as M
import qualified RayTracer.Data.Pattern as P
import qualified RayTracer.Data.Pattern.ColorPattern as CP
import qualified RayTracer.Data.Pattern.StripePattern as SP
import qualified RayTracer.Data.Ray as R
import qualified RayTracer.Data.Shape as S
import qualified RayTracer.Data.Shape.Sphere as SS
import qualified RayTracer.Data.Tuple as T
import RayTracer.Projectile
import RayTracer.Transformation

data Pattern a
  = ColorPattern (CP.ColorPattern a)
  | StripePattern (SP.StripePattern a)
  deriving (Eq, Show)

colorPattern :: Num a => C.Color a -> Pattern a
colorPattern = ColorPattern . CP.colorPattern

stripePattern :: RealFrac a => C.Color a -> C.Color a -> Pattern a
stripePattern a = StripePattern . SP.stripePattern a

instance (RealFrac a) => P.Pattern Pattern a where
  transformation (ColorPattern a) = P.transformation a
  transformation (StripePattern a) = P.transformation a
  patternAt (ColorPattern a) = P.patternAt a
  patternAt (StripePattern a) = P.patternAt a

material :: (RealFrac a) => M.Material Pattern a
material = M.Material (colorPattern C.white) 0.1 0.9 0.9 200.0

sphere :: (Num a, RealFrac a) => SS.Sphere Pattern a
sphere = SS.Sphere identity material

main :: [String]
main = [show finalCanvas]
  where
    rayOrigin = T.point 0 0 (-5)
    wallZ = 10.0
    wallSize = 7.0
    canvasPixels = 100
    pixelSize = wallSize / fromIntegral canvasPixels
    half = wallSize / 2
    initialCanvas = C.canvas (canvasPixels, canvasPixels)
    color = fuchsia
    shape = sphere

    pixels =
      filter isHit $
        (`quotRem` canvasPixels)
          <$> [0 .. canvasPixels * canvasPixels - 1]

    isHit (x, y) = isJust $ I.hit xs
      where
        worldX = (- half) + pixelSize * fromIntegral x
        worldY = half - pixelSize * fromIntegral y
        position = T.point worldX worldY wallZ
        r = R.ray rayOrigin (T.normalize (position - rayOrigin))
        xs = r `S.intersect` shape

    finalCanvas = updateCanvas color initialCanvas pixels
