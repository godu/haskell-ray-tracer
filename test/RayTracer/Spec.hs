{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module RayTracer.Spec
  ( Pattern,
    colorPattern,
    stripePattern,
    sphere,
    material,
    world,
  )
where

import qualified RayTracer.Data.Color as C
import qualified RayTracer.Data.Light as L
import qualified RayTracer.Data.Material as M
import qualified RayTracer.Data.Pattern as P
import qualified RayTracer.Data.Pattern.ColorPattern as CP
import qualified RayTracer.Data.Pattern.StripePattern as SP
import qualified RayTracer.Data.Shape.Sphere as S
import qualified RayTracer.Data.Tuple as T
import qualified RayTracer.Data.World as W
import RayTracer.Transformation

data Pattern a
  = ColorPattern (CP.ColorPattern a)
  | StripePattern (SP.StripePattern a)
  deriving (Eq, Show)

colorPattern :: Num a => C.Color a -> Pattern a
colorPattern = ColorPattern . CP.colorPattern

stripePattern :: RealFrac a => C.Color a -> C.Color a -> Pattern a
stripePattern a = StripePattern . SP.stripePattern a

instance (RealFrac a) => P.Pattern Pattern a where
  transformation (ColorPattern a) = P.transformation a
  transformation (StripePattern a) = P.transformation a
  patternAt (ColorPattern a) = P.patternAt a
  patternAt (StripePattern a) = P.patternAt a

material :: (RealFrac a) => M.Material Pattern a
material = M.Material (colorPattern C.white) 0.1 0.9 0.9 200.0

sphere :: (Num a, RealFrac a) => S.Sphere Pattern a
sphere = S.Sphere identity material

world :: (Fractional a, RealFrac a) => W.World (S.Sphere Pattern) a
world = W.World [s1, s2] (return l)
  where
    s1 =
      sphere
        { S.material =
            material
              { M.pattern_ = colorPattern $ C.color 0.8 1.0 0.6,
                M.diffuse = 0.7,
                M.specular = 0.2
              }
        }
    s2 =
      sphere
        { S.transformation = scaling 0.5 0.5 0.5
        }
    l = L.pointLight (T.point (-10) 10 (-10)) (C.color 1 1 1)
