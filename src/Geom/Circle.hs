module Geom.Circle
  ( Circle(..)
  , circumPoint
  , circumPhase
  , overlap
  ) where

import Data.RVar
import Data.Random.Distribution.Uniform
import Data.Random.Distribution.Uniform
import Linear

import Patterns.Sampling

data Circle =
  Circle (V2 Double)
         Double

instance Sample Circle where
  spatialSample (Circle p r) = do
    theta <- sampleRVar $ uniform 0 (2 * pi)
    r' <- sampleRVar $ uniform 0 r
    return $ circumPoint p theta r'

overlap :: Circle -> Circle -> Double
overlap (Circle c1 r1) (Circle c2 r2) = r1 + r2 - distance c1 c2

circumPhase :: V2 Double -> V2 Double -> Double
circumPhase (V2 cx cy) (V2 x y) = atan2 (y - cy) (x - cx)

circumPoint :: V2 Double -> Double -> Double -> V2 Double
circumPoint (V2 x y) theta r = (V2 (x + r * cos theta) (y + r * sin theta))
