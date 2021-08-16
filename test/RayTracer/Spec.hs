{-# OPTIONS_GHC -Wno-unused-imports #-}

module RayTracer.Spec
  ( Pattern,
    colorPattern,
    stripePattern,
    sphere,
    material,
    world,
    plane,
    Shape (Sphere, Plane),
  )
where

import qualified RayTracer.Data.Color as C
import RayTracer.Data.Intersection
import qualified RayTracer.Data.Light as L
import qualified RayTracer.Data.Material as M
import qualified RayTracer.Data.Pattern as P
import qualified RayTracer.Data.Pattern.ColorPattern as CP
import qualified RayTracer.Data.Pattern.StripePattern as SP
import qualified RayTracer.Data.Shape as SS
import qualified RayTracer.Data.Shape.Plane as P
import qualified RayTracer.Data.Shape.Sphere as S
import qualified RayTracer.Data.Tuple as T
import qualified RayTracer.Data.World as W
import RayTracer.Transformation

data Pattern a
  = ColorPattern (CP.ColorPattern a)
  | StripePattern (SP.StripePattern Pattern a)
  deriving (Eq, Show)

colorPattern :: Num a => C.Color a -> Pattern a
colorPattern = ColorPattern . CP.colorPattern

stripePattern :: RealFrac a => Pattern a -> Pattern a -> Pattern a
stripePattern a = StripePattern . SP.stripePattern a

instance (RealFrac a) => P.Pattern Pattern a where
  getTransformation (ColorPattern p) = P.getTransformation p
  getTransformation (StripePattern p) = P.getTransformation p
  setTransformation (ColorPattern p) = ColorPattern . P.setTransformation p
  setTransformation (StripePattern p) = StripePattern . P.setTransformation p
  patternAt (ColorPattern a) = P.patternAt a
  patternAt (StripePattern a) = P.patternAt a

material :: (RealFrac a) => M.Material Pattern a
material = M.Material (colorPattern C.white) 0.1 0.9 0.9 200.0 0.0

sphere :: (RealFrac a) => S.Sphere Pattern a
sphere = S.Sphere identity material

plane :: (RealFrac a) => P.Plane Pattern a
plane = P.Plane identity material

data Shape p a
  = Sphere (S.Sphere p a)
  | Plane (P.Plane p a)
  deriving (Show, Eq)

instance (Num a, Floating a, Ord a, RealFrac a) => SS.Shape Shape Pattern a where
  transformation (Plane p) = SS.transformation p
  transformation (Sphere s) = SS.transformation s
  material (Plane p) = SS.material p
  material (Sphere s) = SS.material s
  localIntersect r (Plane p) =
    (\(Intersection t p) -> intersection t (Plane p))
      <$> r `SS.localIntersect` p
  localIntersect r (Sphere s) =
    (\(Intersection t s) -> intersection t (Sphere s))
      <$> r `SS.localIntersect` s
  localNormalAt (Plane p) = SS.localNormalAt p
  localNormalAt (Sphere s) = SS.localNormalAt s

world :: (Fractional a, RealFrac a) => W.World (S.Sphere Pattern) a
world = W.World [s1, s2] [l]
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
