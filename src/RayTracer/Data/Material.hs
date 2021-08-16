module RayTracer.Data.Material
  ( Material (Material, pattern_, ambient, diffuse, specular, shininess, reflective),
  )
where

data Material p a = Material
  { pattern_ :: !(p a),
    ambient :: !a,
    diffuse :: !a,
    specular :: !a,
    shininess :: !a,
    reflective :: !a
  }
  deriving (Show, Eq)
