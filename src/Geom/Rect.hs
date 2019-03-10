module Geom.Rect
  ( Rect(..)
  , fullFrame
  , rectCenter
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
  }

fullFrame :: Generate Rect
fullFrame = do
  World {width, height, ..} <- asks world
  return $ Rect (V2 0 0) width height

rectCenter :: Rect -> V2 Double
rectCenter (Rect (V2 x y) w h) = V2 (x + w / 2) (y + h / 2)
