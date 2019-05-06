module Generate.Colour
  ( CairoColour(..)
  , RadialRamp(..)
  , LinearRamp(..)
  , hexcolour
  , setColour
  ) where

import Data.Colour
import Data.Colour.RGBSpace
import Data.Colour.SRGB
import Data.Colour.SRGB.Linear
import Graphics.Rendering.Cairo
import Linear

class CairoColour d where
  withColour :: d -> (Pattern -> Render ()) -> Render ()

instance CairoColour (RGB Double) where
  withColour col = withRGBPattern channelRed channelGreen channelBlue
    where
      RGB {..} = toSRGB $ uncurryRGB rgb $ col

instance CairoColour (RGB Double, Double) where
  withColour (col, alpha) =
    withRGBAPattern channelRed channelGreen channelBlue alpha
    where
      RGB {..} = toSRGB $ uncurryRGB rgb $ col

instance CairoColour String where
  withColour string = withColour $ hexcolour string

instance CairoColour (Double, Double, Double, Double) where
  withColour (r, g, b, alpha) = withRGBAPattern r g b alpha

instance CairoColour (Colour Double) where
  withColour col = withRGBPattern channelRed channelGreen channelBlue
    where
      RGB {..} = toSRGB col

data RadialRamp = RadialRamp
  { radialRampStart :: (V2 Double, Double)
  , radialRampEnd :: (V2 Double, Double)
  , radialRampStops :: [(Double, RGB Double, Double)]
  }

instance CairoColour RadialRamp where
  withColour (RadialRamp (s@(V2 sx sy), sr) (e@(V2 ex ey), er) stops) f =
    withRadialPattern sx sy sr ex ey er $ \pattern ->
      prepare pattern >> f pattern
    where
      prepare :: Pattern -> Render ()
      prepare pattern = foldr1 (>>) $ map (formatStop pattern) stops
      formatStop :: Pattern -> (Double, RGB Double, Double) -> Render ()
      formatStop pattern (offset, col, alpha) =
        patternAddColorStopRGBA
          pattern
          offset
          channelRed
          channelGreen
          channelBlue
          alpha
        where
          RGB {..} = toSRGB $ uncurryRGB rgb $ col

data LinearRamp = LinearRamp
  { rampStart :: V2 Double
  , rampEnd :: V2 Double
  , rampStops :: [(Double, RGB Double, Double)]
  }

instance CairoColour LinearRamp where
  withColour (LinearRamp s@(V2 sx sy) e@(V2 ex ey) stops) f =
    withLinearPattern sx sy ex ey $ \pattern -> prepare pattern >> f pattern
    where
      prepare :: Pattern -> Render ()
      prepare pattern = foldr1 (>>) $ map (formatStop pattern) stops
      formatStop :: Pattern -> (Double, RGB Double, Double) -> Render ()
      formatStop pattern (offset, col, alpha) =
        patternAddColorStopRGBA
          pattern
          offset
          channelRed
          channelGreen
          channelBlue
          alpha
        where
          RGB {..} = toSRGB $ uncurryRGB rgb $ col

setColour :: CairoColour c => c -> Render ()
setColour col = withColour col $ \pattern -> setSource pattern

hexcolour :: String -> RGB Double
hexcolour = toSRGB . sRGB24read
