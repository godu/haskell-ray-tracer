module RayTracer.Data.Pattern
  ( Pattern,
    getTransformation,
    setTransformation,
    patternAt,
  )
where

import RayTracer.Data.Color (Color)
import RayTracer.Data.Matrix (Matrix)
import RayTracer.Data.Tuple (Tuple)

class (Eq (p a)) => Pattern p a where
  getTransformation :: p a -> Matrix a
  setTransformation :: p a -> Matrix a -> p a
  patternAt :: p a -> Tuple a -> Maybe (Color a)
