module Geom.Circle
  ( circumPoint
  , circumPhase
  ) where

import Linear

circumPhase :: V2 Double -> V2 Double -> Double
circumPhase (V2 cx cy) (V2 x y) = atan2 (y - cy) (x - cx)

circumPoint :: V2 Double -> Double -> Double -> V2 Double
circumPoint (V2 x y) theta r = (V2 (x + r * cos theta) (y + r * sin theta))
