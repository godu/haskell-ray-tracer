module RayTracer.Data.Pattern.Extra
  ( patternAtShape,
  )
where

import qualified RayTracer.Data.Color as C
import RayTracer.Data.Matrix
import RayTracer.Data.Pattern
import qualified RayTracer.Data.Shape as S
import qualified RayTracer.Data.Tuple as T

patternAtShape :: (S.Shape o p a, RealFrac a) => p a -> o p a -> T.Tuple a -> Maybe (C.Color a)
patternAtShape pattern_ object worldPoint = patternAt pattern_ <$> patternPoint
  where
    objectPoint = (*^ worldPoint) <$> inverse (S.transformation object)
    patternPoint = (*^) <$> inverse (transformation pattern_) <*> objectPoint
