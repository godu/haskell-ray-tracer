module RayTracer.Chapter5
  ( main,
    Pattern
      ( ColorPattern,
        StripePattern,
        GradientPattern,
        RadialGradientPattern,
        RingPattern,
        CheckersPattern,
        BlendedPattern,
        PerturbedPattern
      ),
    colorPattern,
    stripePattern,
    ringPattern,
    gradientPattern,
    radialGradientPattern,
    checkersPattern,
    blendedPattern,
    perturbedPattern,
  )
where

import Data.Maybe (isJust)
import RayTracer.Data.Canvas (canvas)
import RayTracer.Data.Color (Color, white)
import RayTracer.Data.Intersection (hit)
import RayTracer.Data.Material (Material (Material))
import qualified RayTracer.Data.Pattern as P (Pattern (getTransformation, patternAt, setTransformation))
import qualified RayTracer.Data.Pattern.BlendedPattern as BP (BlendedPattern, blendedPattern)
import qualified RayTracer.Data.Pattern.CheckersPattern as CP (CheckersPattern, checkersPattern)
import qualified RayTracer.Data.Pattern.ColorPattern as CP (ColorPattern, colorPattern)
import qualified RayTracer.Data.Pattern.GradientPattern as GP (GradientPattern, gradientPattern)
import qualified RayTracer.Data.Pattern.PerturbedPattern as PP (PerturbedPattern, perturbedPattern)
import qualified RayTracer.Data.Pattern.RadialGradientPattern as RGP (RadialGradientPattern, radialGradientPattern)
import qualified RayTracer.Data.Pattern.RingPattern as RP (RingPattern, ringPattern)
import qualified RayTracer.Data.Pattern.StripePattern as SP (StripePattern, stripePattern)
import RayTracer.Data.Ray as R (ray)
import qualified RayTracer.Data.Shape as S (Shape (intersect))
import qualified RayTracer.Data.Shape.Sphere as SS (Sphere (Sphere))
import RayTracer.Data.Tuple as T (normalize, point)
import RayTracer.Projectile (fuchsia, updateCanvas)
import RayTracer.Transformation (identity)

data Pattern a
  = ColorPattern (CP.ColorPattern a)
  | StripePattern (SP.StripePattern Pattern a)
  | GradientPattern (GP.GradientPattern Pattern a)
  | RadialGradientPattern (RGP.RadialGradientPattern Pattern a)
  | RingPattern (RP.RingPattern Pattern a)
  | CheckersPattern (CP.CheckersPattern Pattern a)
  | BlendedPattern (BP.BlendedPattern Pattern a)
  | PerturbedPattern (PP.PerturbedPattern Pattern a)
  deriving (Eq, Show)

colorPattern :: Num a => Color a -> Pattern a
colorPattern = ColorPattern . CP.colorPattern

stripePattern :: RealFrac a => Pattern a -> Pattern a -> Pattern a
stripePattern a = StripePattern . SP.stripePattern a

ringPattern :: RealFrac a => Pattern a -> Pattern a -> Pattern a
ringPattern a = RingPattern . RP.ringPattern a

gradientPattern :: RealFrac a => Pattern a -> Pattern a -> Pattern a
gradientPattern a = GradientPattern . GP.gradientPattern a

radialGradientPattern :: RealFrac a => Pattern a -> Pattern a -> Pattern a
radialGradientPattern a = RadialGradientPattern . RGP.radialGradientPattern a

checkersPattern :: (RealFrac a) => Pattern a -> Pattern a -> Pattern a
checkersPattern a = CheckersPattern . CP.checkersPattern a

blendedPattern :: (RealFrac a) => Pattern a -> Pattern a -> Pattern a
blendedPattern a = BlendedPattern . BP.blendedPattern a

perturbedPattern :: (RealFrac a) => Pattern a -> Pattern a -> Pattern a
perturbedPattern a = PerturbedPattern . PP.perturbedPattern a

instance (RealFrac a, Floating a) => P.Pattern Pattern a where
  getTransformation (ColorPattern a) = P.getTransformation a
  getTransformation (StripePattern a) = P.getTransformation a
  getTransformation (RingPattern a) = P.getTransformation a
  getTransformation (GradientPattern a) = P.getTransformation a
  getTransformation (RadialGradientPattern a) = P.getTransformation a
  getTransformation (CheckersPattern a) = P.getTransformation a
  getTransformation (BlendedPattern a) = P.getTransformation a
  getTransformation (PerturbedPattern a) = P.getTransformation a
  setTransformation (ColorPattern a) = ColorPattern . P.setTransformation a
  setTransformation (StripePattern a) = StripePattern . P.setTransformation a
  setTransformation (RingPattern a) = RingPattern . P.setTransformation a
  setTransformation (GradientPattern a) = GradientPattern . P.setTransformation a
  setTransformation (RadialGradientPattern a) = RadialGradientPattern . P.setTransformation a
  setTransformation (CheckersPattern a) = CheckersPattern . P.setTransformation a
  setTransformation (BlendedPattern a) = BlendedPattern . P.setTransformation a
  setTransformation (PerturbedPattern a) = PerturbedPattern . P.setTransformation a
  patternAt (ColorPattern a) = P.patternAt a
  patternAt (StripePattern a) = P.patternAt a
  patternAt (RingPattern a) = P.patternAt a
  patternAt (GradientPattern a) = P.patternAt a
  patternAt (RadialGradientPattern a) = P.patternAt a
  patternAt (CheckersPattern a) = P.patternAt a
  patternAt (BlendedPattern a) = P.patternAt a
  patternAt (PerturbedPattern a) = P.patternAt a

material :: (RealFrac a) => Material Pattern a
material = Material (colorPattern white) 0.1 0.9 0.9 200.0 0.0

sphere :: (RealFrac a) => SS.Sphere Pattern a
sphere = SS.Sphere identity material

main :: [String]
main = [show finalCanvas]
  where
    rayOrigin = point 0 0 (-5)
    wallZ = 10.0
    wallSize = 7.0
    canvasPixels = 100
    pixelSize = wallSize / fromIntegral canvasPixels
    half = wallSize / 2
    initialCanvas = canvas (canvasPixels, canvasPixels)
    color = fuchsia
    shape = sphere

    pixels =
      filter isHit $
        (`quotRem` canvasPixels)
          <$> [0 .. canvasPixels * canvasPixels - 1]

    isHit (x, y) = isJust $ hit xs
      where
        worldX = (- half) + pixelSize * fromIntegral x
        worldY = half - pixelSize * fromIntegral y
        position = point worldX worldY wallZ
        r = R.ray rayOrigin (normalize (position - rayOrigin))
        xs = r `S.intersect` shape

    finalCanvas = updateCanvas color initialCanvas pixels
