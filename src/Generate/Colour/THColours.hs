module Generate.Colour.THColours
  ( THColours
  , mkTHColours
  , assignTHColour
  ) where

import Control.Monad.ST
import Data.Colour.RGBSpace.HSV
import Data.Colour.SRGB
import Data.Ord
import qualified Data.Vector as V
import qualified Data.Vector.Algorithms.Insertion as VI
import qualified Data.Vector.Unboxed as VU
import Linear

import qualified Generate.Algo.QuadTree as Q
import Generate.Colour.SimplePalette
import Generate.Geom.Rect
import Generate.Monad
import Generate.Patterns.Sampling

data THColours = THColours
  { huePoints :: V.Vector (V2 Double, Double)
  , valuePoints :: V.Vector (V2 Double, Double)
  , palette :: V.Vector (RGB Double)
  }

mkTHColours :: SimplePalette -> Generate THColours
mkTHColours (SimplePalette _ fgPalette) = do
  frame <- fullFrame
  huePoints <- V.sequence $ V.generate 40 $ const $ spatialSample frame
  valuePoints <- V.sequence $ V.generate 40 $ const $ spatialSample frame
  huePoints' <-
    V.sequence $
    V.map (\p -> randElem fgPalette >>= \c -> return (p, hue c)) huePoints
  valuePoints' <-
    V.sequence $
    V.map (\p -> randElem fgPalette >>= \c -> return (p, value c)) valuePoints
  return $ THColours huePoints' valuePoints' fgPalette

closest :: Int -> V2 Double -> V.Vector (V2 Double, Double) -> V.Vector (Double)
closest n focus points = V.slice 0 n $ V.map (snd) $ V.backpermute points idx
  where
    idx = V.convert $ VU.modify (VI.sortBy comp) init
    comp =
      comparing $ \i ->
        let (p, _) = points V.! i
         in distance p focus
    init = VU.generate (V.length points) id

assignTHColour :: THColours -> V2 Double -> Generate (RGB Double)
assignTHColour (THColours huePoints valuePoints palette) p = do
  hue <- randElem $ closest 5 p huePoints
  value <- randElem $ closest 5 p valuePoints
  sat <- randElem palette >>= return . saturation
  return $ hsv hue sat value
