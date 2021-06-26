module RayTracer.Data.Pattern.Extra
  ( patternAtShape,
    patternAtPattern,
  )
where

import RayTracer.Data.Color (Color)
import RayTracer.Data.Matrix (inverse, (*^))
import RayTracer.Data.Pattern (Pattern (getTransformation, patternAt))
import RayTracer.Data.Shape (Shape (transformation))
import RayTracer.Data.Tuple (Tuple)

patternAtShape :: (Shape o p a, RealFrac a) => p a -> o p a -> Tuple a -> Maybe (Color a)
patternAtShape pattern_ object worldPoint = patternAt pattern_ =<< patternPoint
  where
    objectPoint = (*^ worldPoint) <$> inverse (transformation object)
    patternPoint = (*^) <$> inverse (getTransformation pattern_) <*> objectPoint

patternAtPattern :: (Pattern p a, Fractional a, Eq a) => p a -> Tuple a -> Maybe (Color a)
patternAtPattern pattern_ point = patternAt pattern_ =<< patternPoint
  where
    objectPoint = (*^ point) <$> inverse (getTransformation pattern_)
    patternPoint = (*^) <$> inverse (getTransformation pattern_) <*> objectPoint
