module Generate.Geom.Rect
  ( Rect(..)
  , fullFrame
  , rectCenter
  , withinRect
  , distanceToRect
  ) where

import Control.Monad.Reader
import Data.Maybe
import Data.RVar
import Data.RVar
import Data.Random.Distribution.Uniform
import qualified Data.Vector as V
import Graphics.Rendering.Cairo
import Linear
import Test.Hspec

import Generate.Coord
import Generate.Draw
import Generate.Geom
import Generate.Geom.Circle
import Generate.Geom.Line
import Generate.Monad
import Generate.Patterns.Sampling

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

instance Center Rect where
  center (Rect (V2 tlx tly) w h) = V2 (tlx + w / 2) (tly + h / 2)

instance Scale Rect where
  scaleFrom factor anchor (Rect topLeft w h) =
    let phase = circumPhase anchor topLeft
        dist = distance topLeft anchor
        topLeft' = circumPoint anchor phase $ dist * factor
     in Rect topLeft' (w * factor) (h * factor)

instance Drawable Rect where
  draw (Rect (V2 tlx tly) w h) = do
    rectangle tlx tly w h

instance Lines Rect where
  toLines (Rect tl@(V2 tlx tly) w h) =
    V.fromList $
    map
      (fromJust . fromVertices . V.fromList)
      [ [tl, V2 tlx (tly + h)]
      , [tl, V2 (tlx + w) tly]
      , [V2 (tlx + w) tly, V2 (tlx + w) (tly + h)]
      , [V2 (tlx + w) (tly + h), V2 tlx (tly + h)]
      ]

instance Split Rect where
  splitOnAxis axis t (Rect tl@(V2 x y) w h) = (Rect tl w1 h1, Rect tl2 w2 h2)
    where
      tl2 =
        case axis of
          X -> V2 (x + w1) y
          Y -> V2 x (y + h1)
      (w1, w2) =
        case axis of
          X -> (w * t, w * (1 - t))
          Y -> (w, w)
      (h1, h2) =
        case axis of
          X -> (h, h)
          Y -> (h * t, h * (1 - t))

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
