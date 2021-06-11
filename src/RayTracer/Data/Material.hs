module RayTracer.Data.Material
  ( Material (Material, pattern_, ambient, diffuse, specular, shininess),
  )
where

data Material p a = Material
  { pattern_ :: !(p a),
    ambient :: !a,
    diffuse :: !a,
    specular :: !a,
    shininess :: !a
  }
  deriving (Show, Eq)
