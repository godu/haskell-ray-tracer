module RayTracer.Data.Pattern.GradientPattern
  ( GradientPattern (GradientPattern, transformation, a, b),
    gradientPattern,
  )
where

import qualified RayTracer.Data.Color as C ((*^))
import qualified RayTracer.Data.Matrix as M (Matrix)
import RayTracer.Data.Pattern (Pattern (..))
import RayTracer.Data.Pattern.Extra (patternAtPattern)
import RayTracer.Data.Tuple (Tuple (Tuple))
import RayTracer.Transformation (identity)

data GradientPattern p a = GradientPattern
  { transformation :: !(M.Matrix a),
    a :: !(p a),
    b :: !(p a)
  }
  deriving (Eq, Show)

gradientPattern :: Num a => p a -> p a -> GradientPattern p a
gradientPattern = GradientPattern identity

instance (RealFrac a, Pattern p a) => Pattern (GradientPattern p) a where
  getTransformation = transformation
  setTransformation p t = p {transformation = t}
  patternAt (GradientPattern _ a b) p@(Tuple x _ _ _) = do
    a' <- patternAtPattern a p
    b' <- patternAtPattern b p
    let distance = b' - a'
        fraction = x - fromIntegral (floor x)
    pure $ a' + (distance C.*^ fraction)