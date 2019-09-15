module Generate.Patterns.Water
  ( Translucent(..)
  , WaterColourCfg(..)
  , Splotch
  , mkSplotch
  , warp
  , warpN
  , flatWaterColour
  , waterColour
  ) where

import "monad-extras" Control.Monad.Extra
import Data.Colour.SRGB
import Data.Default
import Data.Maybe
import Data.RVar
import Data.Random.Distribution.Normal
import Data.Random.Distribution.Uniform
import qualified Data.Vector as V
import Graphics.Rendering.Cairo
import Linear

import Generate.Colour
import Generate.Draw
import Generate.Geom
import Generate.Geom.Circle
import Generate.Geom.Line
import Generate.Monad
import Generate.Patterns.Wiggle

class Translucent t where
  setOpacity :: Double -> t -> t

data Splotch =
  Splotch
    { poly :: Line
    , colour :: (RGB Double, Double)
    }

instance Wiggle Splotch where
  wiggle w s@(Splotch poly _) = do
    poly' <- wiggle w poly
    return $ s {poly = poly'}

instance Translucent Splotch where
  setOpacity o s@(Splotch _ (c, _)) = s {colour = (c, o)}

instance Subdivisible Splotch where
  subdivide s@(Splotch poly _) = s {poly = subdivide poly}

instance Drawable Splotch where
  draw (Splotch poly c) = do
    setColour c
    draw poly

mkSplotch :: Line -> RGB Double -> Splotch
mkSplotch poly colour = Splotch poly (colour, 1.0)

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
  -> (Int -> Generate Wiggler)
  -> wc
  -> Generate [wc]
flatWaterColour opacity layerCount wiggler source = do
  wigglers <- sequence $ map wiggler [1 .. layerCount]
  layers <- sequence $ map (\w -> wiggle w source) wigglers
  return $ map (setOpacity opacity) layers

data WaterColourCfg =
  WaterColourCfg
    { layerCount :: Int
    , layerOpacity :: Double
    , depthOfBranch :: Int
    , depthPerBranch :: Int
    }

instance Default WaterColourCfg where
  def =
    WaterColourCfg
      { layerCount = 10
      , layerOpacity = 0.1
      , depthOfBranch = 3
      , depthPerBranch = 1
      }

waterColour ::
     (Translucent wc, Wiggle wc, Subdivisible wc)
  => Wiggler
  -> WaterColourCfg
  -> wc
  -> Generate [wc]
waterColour wiggler (WaterColourCfg layerCount opacity depthOfBranch depthPerBranch) src = do
  base <- warpN wiggler depthOfBranch src
  layers <-
    sequence $ map (const $ warpN wiggler depthPerBranch base) [1 .. layerCount]
  return $ map (setOpacity opacity) layers
