module Geom.Rect
  ( Rect(..)
  , fullFrame
  , rectCenter
  , withinRect
  , distanceToRect
  ) where

import Control.Monad.Reader
import Data.RVar
import Data.RVar
import Data.Random.Distribution.Uniform
import Linear
import Test.Hspec

import Generate
import Patterns.Sampling

data Rect = Rect
  { topLeft :: V2 Double
  , rectWidth :: Double
  , rectHeight :: Double
  } deriving (Eq, Show)

instance Sample Rect where
  spatialSample (Rect (V2 tlx tly) w h) = do
    x <- sampleRVar $ uniform 0 1
    y <- sampleRVar $ uniform 0 1
    return $ V2 (tlx + x * w) (tly + y * h)

fullFrame :: Generate Rect
fullFrame = do
  World {width, height, ..} <- asks world
  return $ Rect (V2 0 0) width height

withinRect :: Rect -> V2 Double -> Bool
withinRect (Rect (V2 x y) w h) (V2 px py) =
  px >= x && px <= x + w && py >= y && py <= y + h

rectCenter :: Rect -> V2 Double
rectCenter (Rect (V2 x y) w h) = V2 (x + w / 2) (y + h / 2)

distanceToRect :: Rect -> V2 Double -> Double
distanceToRect r@(Rect (V2 tlx tly) w h) (V2 x y) =
  let xc1 = tlx + w
      xc2 = tlx
      yc1 = tly + h
      yc2 = tly
      xDist = max 0 $ max (x - xc1) (xc2 - x)
      yDist = max 0 $ max (y - yc1) (yc2 - y)
   in norm (V2 xDist yDist)
