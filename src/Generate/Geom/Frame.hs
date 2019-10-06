module Generate.Geom.Frame
  ( Frame
  , mkFrame
  ) where

import Control.Monad.Loops
import Data.Fixed
import Data.RVar
import Data.Random.Distribution.Uniform
import Graphics.Rendering.Cairo
import Linear

import Generate.Draw
import Generate.Geom
import Generate.Geom.Rect
import Generate.Monad
import Generate.Patterns.Sampling

data Frame = Frame
  -- | The outer boundaries of the frame.
  { outer :: Rect
  , thickness :: Double
  }

instance Draw Frame where
  draw frame@(Frame outer thickness) = do
    setSourceRGB 1 0 0
    draw outer
    stroke
    setSourceRGB 0 0 1
    draw $ innerRect frame
    stroke

instance Sample Frame where
  spatialSample frame@(Frame rect t) = do
    let disallowed = innerRect frame
    -- This could be smarter...
    iterateUntil (\c -> not $ withinRect disallowed c) (spatialSample rect)

sampleInRangeExcludeCenter :: Double -> Generate Double
sampleInRangeExcludeCenter r = do
  v <- sampleRVar $ uniform 0 r
  let rightEdge = 1.0 - (r / 2)
  return $ (rightEdge + v) `mod'` 1.0

innerRect :: Frame -> Rect
innerRect (Frame outer thickness) = scaleFromCenter thickness outer

mkFrame ::
     Rect -- ^ The outer boundaries of the frame.
  -> Double -- ^ How thick the frame is in percentage. 0 thickness
            -- is invisible. 0.1 is a frame covering 1/10th of its area.
            -- 1.0 is an opaque square.
  -> Frame
mkFrame outer thickness = Frame outer $ 1.0 - thickness
