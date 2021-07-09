module RayTracer.Data.Pattern
  ( Pattern,
    transformation,
    patternAt,
  )
where

import RayTracer.Data.Color
import RayTracer.Data.Matrix
import qualified RayTracer.Data.Tuple as T

class (Eq (p a)) => Pattern p a where
  transformation :: p a -> Matrix a
  patternAt :: p a -> T.Tuple a -> Color a
