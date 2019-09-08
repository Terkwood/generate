module Main where

import qualified Data.Vector as V
import Linear hiding (rotate)
import System.IO.Unsafe

import Generate
import qualified Generate.Algo.QuadTree as Q
import qualified Generate.Algo.Vec as V
import Generate.Colour.SimplePalette
import Generate.Colour.THColours
import Generate.Geom.Frame
import Generate.Patterns.Grid
import Generate.Patterns.Maze
import Generate.Patterns.Sampling
import Generate.Patterns.Water
import Petal

ramp :: Int -> [Double]
ramp total = map valueOf [0 .. total]
  where
    valueOf i = (fromIntegral i) / (fromIntegral total)

mkPalette :: Generate SimplePalette
mkPalette =
  randElem $
  V.fromList
    [ jhoto
    , metroid
    , mkSimplePalette "EFC271" ["3E8A79", "E9A931", "F03E4D", "CC3433"]
    ]

background :: SimplePalette -> Generate (Render ())
background palette = do
  World {..} <- asks world
  return $ do
    setColour $ bgColour palette
    rectangle 0 0 width height
    fill

main :: IO ()
main = do
  runInvocation $ do
    palette <- mkPalette
    fgPalette <- mkTHColours palette
    bg <- background palette
    return $ bg
