module Geom.Rect
  ( Rect(..)
  , fullFrame
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
