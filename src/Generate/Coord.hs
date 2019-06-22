module Generate.Coord
  ( randomPoint
  , centerPoint
  , midpoint
  , moveToward
  , Axis(..)
  ) where

import Control.Monad.Reader
import Data.RVar
import Data.Random.Distribution.Uniform
import Generate.Geom.Circle
import Generate.Monad
import Linear

data Axis
  = X
  | Y

randomPoint :: Generate (V2 Double)
randomPoint = do
  World {..} <- asks world
  x <- sampleRVar $ uniform 0 1
  y <- sampleRVar $ uniform 0 1
  return $ V2 (x * width) (y * height)

centerPoint :: Generate (V2 Double)
centerPoint = do
  World {..} <- asks world
  return $ V2 (width / 2) (height / 2)

midpoint :: V2 Double -> V2 Double -> V2 Double
midpoint p1 p2 = (p1 + p2) / 2

moveToward :: V2 Double -> V2 Double -> Double -> V2 Double
moveToward c p d =
  let theta = circumPhase c p
      r = distance c p
   in circumPoint c theta (r - d)
