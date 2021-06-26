module RayTracer.Chapter8
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
  ( Material (ambient, diffuse, pattern_, shininess, specular),
  )
import qualified RayTracer.Data.Shape.Sphere as S (Sphere (material, transformation))
import RayTracer.Data.Tuple (point, vector)
import RayTracer.Data.World (World (lights, objects), world)
import RayTracer.Transformation
  ( rotationZ,
    scaling,
    translation,
    viewTransform,
  )

main :: [String]
main = [show canvas]
  where
    sphereMaterial =
      material
        { pattern_ = colorPattern (color 1 1 1),
          ambient = 0.2,
          diffuse = 0.8,
          specular = 0.3,
          shininess = 200
        }

    wristMaterial =
      sphereMaterial
        { pattern_ = colorPattern (color 0.1 1 1)
        }

    palmMaterial =
      sphereMaterial
        { pattern_ = colorPattern (color 0.1 0.1 1)
        }

    thumbMaterial =
      sphereMaterial
        { pattern_ = colorPattern (color 0.1 0.1 1)
        }

    indexMaterial =
      sphereMaterial
        { pattern_ = colorPattern (color 1 1 0.1)
        }

    middleMaterial =
      sphereMaterial
        { pattern_ = colorPattern (color 0.1 1 0.1)
        }

    ringMaterial =
      sphereMaterial
        { pattern_ = colorPattern (color 0.1 1 0.1)
        }

    pinkyMaterial =
      sphereMaterial
        { pattern_ = colorPattern (color 0.1 0.5 0.1)
        }

    backdrop :: S.Sphere Pattern Double
    backdrop =
      sphere
        { S.material =
            material
              { pattern_ = colorPattern (color 1 1 1),
                ambient = 0,
                diffuse = 0.5,
                specular = 0
              },
          S.transformation = translation 0 0 20 * scaling 200 200 0.01
        }

    wrist =
      sphere
        { S.material = wristMaterial,
          S.transformation =
            rotationZ (pi / 4)
              * translation (-4) 0 (-21)
              * scaling 3 3 3
        }

    palm =
      sphere
        { S.material = palmMaterial,
          S.transformation = translation 0 0 (-15) * scaling 4 3 3
        }

    thumb =
      sphere
        { S.material = thumbMaterial,
          S.transformation = translation (-2) 2 (-16) * scaling 1 3 1
        }

    index =
      sphere
        { S.material = indexMaterial,
          S.transformation = translation 3 2 (-22) * scaling 3 0.75 0.75
        }

    middle =
      sphere
        { S.material = middleMaterial,
          S.transformation = translation 4 1 (-19) * scaling 3 0.75 0.75
        }

    ring =
      sphere
        { S.material = ringMaterial,
          S.transformation = translation 4 0 (-18) * scaling 3 0.75 0.75
        }

    pinky =
      sphere
        { S.material = pinkyMaterial,
          S.transformation =
            translation 3 (-1.5) (-20)
              * rotationZ (- pi / 10)
              * translation 1 0 0
              * scaling 2.5 0.6 0.6
        }

    world' =
      world
        { objects =
            [ backdrop,
              wrist,
              palm,
              thumb,
              index,
              middle,
              ring,
              pinky
            ],
          lights = [pointLight (point 0 0 (-100)) (color 1 1 1)]
        }

    camera' =
      (camera 400 200 (pi / 6))
        { transformation =
            viewTransform (point 40 0 (-70)) (point 0 0 (-5)) (vector 0 1 0)
        }

    canvas :: Canvas Double
    canvas = render camera' world'
