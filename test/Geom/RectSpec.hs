module Geom.RectSpec
  ( spec
  ) where

import Geom.Rect
import Linear
import Test.Hspec

spec :: Spec
spec =
  describe "distanceToRect" $ do
    it "gives correct distance" $ do
      distanceToRect (Rect (V2 0 0) 100 100) (V2 100 101) `shouldBe` 1
