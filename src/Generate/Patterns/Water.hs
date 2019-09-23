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
import Generate.Geom.Line
import Generate.Monad
import Generate.Transforms.Warp

class Translucent t where
  setOpacity :: Double -> t -> t

data Splotch =
  Splotch
    { poly :: Line
    , colour :: (RGB Double, Double)
    }

instance Warp Splotch where
  warp w s@(Splotch poly _) = do
    poly' <- warp w poly
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