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
  , jhoto
  , monoPastelRed
  , monoPastelPurple
  , monoPastelBlue
  , mondrian
  ) where

import Data.Colour.SRGB
import qualified Data.Vector as V

import Generate.Colour
import Generate.Monad

data SimplePalette =
  SimplePalette (RGB Double) (V.Vector (RGB Double))

mkSimplePalette :: String -> [String] -> SimplePalette
mkSimplePalette bg fgs =
  SimplePalette (hexcolour bg) $ V.fromList $ map hexcolour fgs

redPalette = mkSimplePalette "F8F5EB" ["AD4749", "F8F5EB", "BFAAB9", "AF849B"]

monoPastelRed =
  mkSimplePalette "FFAAAA" ["D46A6A", "AA3939", "801515", "550000"]

monoPastelPurple =
  mkSimplePalette "C17BC1" ["9F4B9F", "892E89", "751975", "5D085D"]

monoPastelBlue =
  mkSimplePalette "96DEDE" ["61C0C0", "3BA2A2", "228F8F", "0C7A7A"]

jhoto =
  mkSimplePalette "F5F6ED" ["D32E40", "D77C32", "D5C06D", "0FAE7B", "21948B"]

castle =
  mkSimplePalette "FFFFFF" ["C74894", "FDDCB7", "F75856", "66AA79", "07444D"]

mondrian = mkSimplePalette "f9f9f9" ["fff001", "ff0101", "101010"]

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
