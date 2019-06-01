module Generate.Colour.SimplePalette
  ( SimplePalette(..)
  , mkSimplePalette
  , fgColour
  , bgColour
  , castle
  , mote
  , metroid
  , gurken
  , redPalette
  ) where

import Data.Colour.SRGB
import qualified Data.Vector as V

import Generate.Colour
import Generate.Monad

data SimplePalette =
  SimplePalette (RGB Double)
                (V.Vector (RGB Double))

mkSimplePalette :: String -> [String] -> SimplePalette
mkSimplePalette bg fgs =
  SimplePalette (hexcolour bg) $ V.fromList $ map hexcolour fgs

redPalette = mkSimplePalette "F8F5EB" ["AD4749", "F8F5EB", "BFAAB9", "AF849B"]

castle =
  mkSimplePalette "FFFFFF" ["C74894", "FDDCB7", "F75856", "66AA79", "07444D"]

mote =
  mkSimplePalette "EECDB6" ["A83250", "13192C", "8D4C33", "E08752", "A83250"]

metroid = mkSimplePalette "ECE9E4" ["4DDCEC", "189CE6", "116FAE", "2D1D39"]

gurken = mkSimplePalette "FCFDF8" ["3FACA5", "7DDCC6", "F79EA2", "FA5778"]

fgColour :: SimplePalette -> Generate (RGB Double)
fgColour palette =
  let SimplePalette _ fgs = palette
   in randElem fgs

bgColour :: SimplePalette -> RGB Double
bgColour (SimplePalette bg _) = bg
