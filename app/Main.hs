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
import qualified Streaming.Prelude as S

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

bgStream :: Stream (Render ())
bgStream = do
  palette <- lift $ mkPalette
  render <- lift $ background palette
  S.yield render

data Dot =
  Dot
    { circle :: Circle
    , value :: Double
    , alpha :: Double
    }

randomCircle :: Generate Dot
randomCircle = do
  frame <- fullFrame >>= return . (scaleFromCenter 0.5)
  center <- spatialSample frame
  value <- sampleRVar $ uniform 0.3 1.0
  alpha <- sampleRVar $ uniform 0.1 1.0
  return $ Dot (Circle center 0.5) value alpha

circles :: Int -> Stream Dot
circles n = streamGenerates $ map (const $ randomCircle) [0 .. n]

drawCircle :: Dot -> Render ()
drawCircle (Dot c v a) = do
  setSourceRGBA v v v a
  draw c
  fill

circleStream :: Stream (Render ())
circleStream = S.map (drawCircle) $ circles 30000

main :: IO ()
main = do
  runInvocation $ bgStream >> circleStream
