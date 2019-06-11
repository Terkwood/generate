module Main where

import qualified Data.Vector as V
import Linear hiding (rotate)
import System.IO.Unsafe

import Generate
import Generate.Colour.SimplePalette
import Generate.Patterns.NoiseWalker

mkPalette :: Generate SimplePalette
mkPalette =
  randElem $
  V.fromList
    [ monoPastelBlue
    , monoPastelRed
    , monoPastelBlue
    , monoPastelPurple
    , jhoto
    , castle
    , metroid
    , gurken
    ]

background :: SimplePalette -> Generate (Render ())
background palette = do
  World {..} <- asks world
  return $ do
    setColour $ bgColour palette
    rectangle 0 0 width height
    fill

data BrushStroke = BrushStroke
  { palette :: SimplePalette
  , squigglyPathCfg :: SquigglyPathCfg
  , thickness :: Int
  , startSpread :: RVar Double
  }

renderStroke :: BrushStroke -> Generate (Render ())
renderStroke (BrushStroke palette cfg thickness spreadM) = do
  let startPoint = do
        theta <- sampleRVar $ uniform 0 $ 2 * pi
        spread <- sampleRVar spreadM
        return $ circumPoint (start cfg) theta spread
  paths <-
    sequence $
    map
      (const $ startPoint >>= \start' -> squigglyPath cfg {start = start'})
      [0 .. thickness]
  colours <- sequence $ map (const $ fgColour palette) paths
  return $ do
    setLineWidth 0.03
    foldr1 (>>) $
      map
        (\(p, c) -> do
           setColour c
           draw p
           stroke) $
      zip paths colours

scene :: SimplePalette -> Generate (Render ())
scene palette = do
  start <- centerPoint
  let cfg =
        SquigglyPathCfg
          { walkerCfg =
              NoiseWalkerCfg {scale = normal 100 0.1, step = normal 1 0.1}
          , length = 1000
          , start = start
          }
  col <- fgColour palette
  renderStroke $ BrushStroke palette cfg 5000 $ normal 0 4

main :: IO ()
main =
  runInvocation $ do
    palette <- mkPalette
    background palette
    scene palette
