module RayTracer.Data.WorldSpec
  ( spec,
  )
where

import RayTracer.Data.World
  ( World (light, objects),
    world,
  )
import Test.Hspec
  ( Spec,
    it,
    shouldBe,
  )
import Prelude
  ( Maybe (Nothing),
    ($),
  )

spec :: Spec
spec = do
  it "Creating a world" $ do
    let actual = world
    objects actual `shouldBe` []
    light actual `shouldBe` Nothing
