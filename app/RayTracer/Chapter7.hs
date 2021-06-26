module RayTracer.Chapter7
  ( main,
  )
where

import RayTracer.Chapter5 (Pattern, colorPattern)
import RayTracer.Chapter6 (material, sphere)
import RayTracer.Data.Camera
  ( Camera (transformation),
    camera,
    render,
  )
import RayTracer.Data.Canvas (Canvas)
import RayTracer.Data.Color (color)
import RayTracer.Data.Light (pointLight)
import RayTracer.Data.Material
  ( Material (diffuse, pattern_, specular),
  )
import qualified RayTracer.Data.Shape.Sphere as S (Sphere (material, transformation))
import RayTracer.Data.Tuple (point, vector)
import RayTracer.Data.World (World (lights, objects), world)
import RayTracer.Transformation
  ( rotationX,
    rotationY,
    scaling,
    translation,
    viewTransform,
  )

main :: [String]
main = [show canvas]
  where
    floor :: S.Sphere Pattern Double
    floor =
      sphere
        { S.transformation = scaling 10 0.01 10,
          S.material =
            material
              { pattern_ = colorPattern (color 1 0.9 0.9),
                specular = 0
              }
        }

    leftWall =
      sphere
        { S.transformation = translation 0 0 5 * rotationY (negate pi / 4) * rotationX (pi / 2) * scaling 10 0.01 10,
          S.material =
            material
              { pattern_ = colorPattern (color 1 0.9 0.9),
                specular = 0
              }
        }

    rightWall =
      sphere
        { S.transformation = translation 0 0 5 * rotationY (pi / 4) * rotationX (pi / 2) * scaling 10 0.01 10,
          S.material =
            material
              { pattern_ = colorPattern (color 1 0.9 0.9),
                specular = 0
              }
        }

    middle =
      sphere
        { S.transformation = translation (-0.5) 1 0.5,
          S.material =
            material
              { pattern_ = colorPattern $ color 0.1 1 0.5,
                diffuse = 0.7,
                specular = 0.3
              }
        }

    right =
      sphere
        { S.transformation = translation 1.5 0.5 (-0.5) * scaling 0.5 0.5 0.5,
          S.material =
            material
              { pattern_ = colorPattern $ color 0.5 1 0.1,
                diffuse = 0.7,
                specular = 0.3
              }
        }

    left =
      sphere
        { S.transformation = translation (-1.5) 0.33 (-0.75) * scaling 0.33 0.33 0.33,
          S.material =
            material
              { pattern_ = colorPattern $ color 1 0.8 0.1,
                diffuse = 0.7,
                specular = 0.3
              }
        }

    world' =
      world
        { objects =
            [ floor,
              leftWall,
              rightWall,
              middle,
              right,
              left
            ],
          lights = [pointLight (point (-10) 10 (-10)) (color 1 1 1)]
        }

    camera' =
      (camera 320 160 (pi / 3))
        { transformation = viewTransform (point 0 1.5 (-5)) (point 0 1 0) (vector 0 1 0)
        }

    canvas :: Canvas Double
    canvas = render camera' world'
