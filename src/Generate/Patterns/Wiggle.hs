module Generate.Patterns.Wiggle
  ( Wiggler(..)
  , Wiggle(..)
  , mkRadialWiggler
  , mkNoiseWiggler
  ) where

import Data.Maybe
import Data.RVar
import Data.Random.Distribution.Normal
import Data.Random.Distribution.Uniform
import qualified Data.Vector as V
import Linear

import Generate.Geom
import Generate.Geom.Circle
import Generate.Geom.Line
import Generate.Monad

data Wiggler =
  Wiggler (V2 Double -> Generate (V2 Double))

class Wiggle w where
  wiggle :: Wiggler -> w -> Generate w

instance Wiggle (V2 Double) where
  wiggle (Wiggler f) p = f p

instance Wiggle Line where
  wiggle w line =
    V.sequence (V.map (wiggle w) $ toVertices line) >>= \vs ->
      return $ fromJust $ fromVertices vs

mkRadialWiggler :: Double -> Wiggler
mkRadialWiggler power =
  Wiggler $ \p -> do
    theta <- sampleRVar $ uniform 0 (2 * pi)
    r <- sampleRVar $ normal 0 power
    return $ circumPoint p theta r

mkNoiseWiggler :: Double -> Double -> Double -> Wiggler
mkNoiseWiggler z strength smoothness =
  Wiggler $ \p@(V2 x y) -> do
    let scale = 1 / smoothness
    let fixSamplePoint = fmap (scale *)
    theta <-
      (noiseSample $ fixSamplePoint $ V3 x y z) >>= return . (\x -> x * 2 * pi)
    r <-
      (noiseSample $ fixSamplePoint $ V3 x y (negate z)) >>=
      return . (* strength)
    return $ circumPoint p theta r
