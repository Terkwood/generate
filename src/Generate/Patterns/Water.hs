module Generate.Patterns.Water
  ( Wiggler(..)
  , Wiggle(..)
  , Translucent(..)
  , WaterColourLayering(..)
  , warp
  , warpN
  , flatWaterColour
  , waterColour
  , radialWiggler
  ) where

import Control.Monad.Extra
import Data.RVar
import Data.Random.Distribution.Normal
import Data.Random.Distribution.Uniform
import Linear

import Generate.Geom
import Generate.Geom.Circle
import Generate.Monad

data Wiggler =
  Wiggler (V2 Double -> Generate (V2 Double))

class Wiggle w where
  wiggle :: Wiggler -> w -> Generate w

instance Wiggle (V2 Double) where
  wiggle (Wiggler f) p = f p

class Translucent t where
  setOpacity :: Double -> t -> t

data WaterColourLayering = WaterColourLayering
  { _waterColourLayeringLayerCount :: Int
  , _waterColourLayeringLayerOpacity :: Double
  , _waterColourLayeringDepthOfBranch :: Int
  , _waterColourLayeringDepthPerBranch :: Int
  }

warp :: (Wiggle w, Subdivisible w) => Wiggler -> w -> Generate w
warp wiggler source = wiggle wiggler $ subdivide source

warpN :: (Wiggle w, Subdivisible w) => Wiggler -> Int -> w -> Generate w
warpN wiggler n source = do
  results <- iterateMaybeM (_warpN wiggler) (n, source)
  if null results
    then return source
    else return $ snd (last results)

_warpN ::
     (Wiggle w, Subdivisible w)
  => Wiggler
  -> (Int, w)
  -> Generate (Maybe (Int, w))
_warpN wiggler (n, source) =
  if n < 1
    then return Nothing
    else do
      warped <- warp wiggler source
      return $ Just (n - 1, warped)

flatWaterColour ::
     (Translucent wc, Wiggle wc)
  => Double
  -> Int
  -> Wiggler
  -> wc
  -> Generate [wc]
flatWaterColour opacity layerCount wiggler source = do
  layers <- sequence $ map (const $ wiggle wiggler source) [1 .. layerCount]
  return $ map (setOpacity opacity) layers

waterColour ::
     (Translucent wc, Wiggle wc, Subdivisible wc)
  => Wiggler
  -> WaterColourLayering
  -> wc
  -> Generate [wc]
waterColour wiggler (WaterColourLayering layerCount opacity depthOfBranch depthPerBranch) src = do
  base <- warpN wiggler depthOfBranch src
  layers <-
    sequence $ map (const $ warpN wiggler depthPerBranch base) [1 .. layerCount]
  return $ map (setOpacity opacity) layers

radialWiggler :: Double -> Wiggler
radialWiggler power =
  Wiggler $ \p -> do
    theta <- sampleRVar $ uniform 0 (2 * pi)
    r <- sampleRVar $ normal 0 power
    return $ circumPoint p theta r
