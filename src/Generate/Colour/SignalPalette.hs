module Generate.Colour.SignalPalette
  ( SignalPalette(..)
  , signalColour
  , mkCosinePalette
  ) where

import Linear

import Generate.Colour

data SignalPalette =
  SignalPalette (Double -> Col)

signalColour :: SignalPalette -> Double -> Col
signalColour (SignalPalette f) t = f t

mkCosinePalette ::
     V3 Double -> V3 Double -> V3 Double -> V3 Double -> SignalPalette
mkCosinePalette a b c d =
  SignalPalette $ \t ->
    Col $ a + (b * cos (fmap ((2 *) . (pi *)) ((fmap (* t) c) + d)))
