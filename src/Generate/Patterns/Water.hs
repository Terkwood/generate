module Generate.Patterns.Water
  ( Translucent(..)
  , Splotch
  , mkSplotch
  , flatWaterColour
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
import Generate.Geom.Shape
import Generate.Monad
import Generate.Transforms.Warp

class Translucent t where
  setOpacity :: Double -> t -> t

data Splotch =
  Splotch
    { shape :: Shape
    , colour :: (RGB Double, Double)
    }

instance Warp Splotch where
  warp w s@(Splotch shape _) = do
    shape' <- warp w shape
    return $ s {shape = shape'}

instance Translucent Splotch where
  setOpacity o s@(Splotch _ (c, _)) = s {colour = (c, o)}

instance Subdivisible Splotch where
  subdivide s@(Splotch shape _) = s {shape = subdivide shape}

instance Draw Splotch where
  draw (Splotch shape c) = do
    setColour c
    draw shape

mkSplotch :: Shape -> RGB Double -> Splotch
mkSplotch shape colour = Splotch shape (colour, 1.0)

flatWaterColour ::
     (Translucent wc, Warp wc)
  => Double
  -> Int
  -> (Int -> Generate Warper)
  -> wc
  -> Generate [wc]
flatWaterColour opacity layerCount warperGen source = do
  warpers <- mapM warperGen [1 .. layerCount]
  layers <- mapM (\w -> warp w source) warpers
  return $ map (setOpacity opacity) layers
