module Geom.Rect
  ( Rect(..)
  , fullFrame
  , rectCenter
  , withinRect
  , distanceToRect
  ) where

import Control.Monad.Reader
import Data.RVar
import Data.Random.Distribution.Uniform
import Linear

import Generate

data Rect = Rect
  { topLeft :: V2 Double
  , rectWidth :: Double
  , rectHeight :: Double
  } deriving (Eq, Show)

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
distanceToRect r@(Rect tl w h) p =
  let p' = p + rectCenter r
      d = abs p' - (V2 w h)
   in norm d
