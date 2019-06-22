module Main where

import qualified Data.Vector as V
import Linear hiding (rotate)
import System.IO.Unsafe

import Generate
import Generate.Colour.SimplePalette
import Generate.Patterns.Bands
import Generate.Patterns.NoiseWalker
import Generate.Patterns.RecursiveSplit
import Generate.Patterns.Water

import PetalStroke

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

data BandBox = BandBox
  { bandLines :: V.Vector Line
  , colour :: (RGB Double, Double)
  }

instance Wiggle BandBox where
  wiggle w b@(BandBox lines _) = do
    lines' <- V.sequence $ V.map (wiggle w) lines
    return $ b {bandLines = lines'}

instance Translucent BandBox where
  setOpacity o' b@(BandBox _ (c, o)) = b {colour = (c, o')}

instance Drawable BandBox where
  draw (BandBox lines c) = do
    setColour c
    drawAll $ V.toList lines
    stroke

bandBox :: SimplePalette -> Rect -> Generate (Render ())
bandBox palette box = do
  c <- fgColour palette
  let bounds = intoBounds $ UniformBands 10
  let box' = scaleFromCenter 0.7 box
  let bands = bandsOnAxis X bounds box'
  let bandBoxes =
        map
          (\band ->
             BandBox (V.map (subdivideN 4) $ toLines band) (c, 1.0 :: Double))
          bands
  let wiggler = radialWiggler 0.2
  bandBoxes' <- sequence $ map (flatWaterColour 0.4 3 wiggler) bandBoxes
  return $ drawAll $ concat bandBoxes'

scene :: SimplePalette -> Generate (Render ())
scene palette = do
  root <- centerPoint
  frame <- fullFrame
  let box = scaleFromCenter 0.8 frame
  colour <- fgColour palette
  let predicate =
        sampleRVar (uniform (negate 1) 1) >>= \(v :: Double) -> return $ v > 0
  boxes <- recursiveSplit def box >>= fairFilter predicate
  bandBoxes <- sequence $ map (bandBox palette) boxes
  return $ do
    setColour colour
    setLineWidth 3
    foldr (>>) (pure ()) bandBoxes

main :: IO ()
main =
  runInvocation $ do
    palette <- mkPalette
    bg <- background palette
    fg <- scene palette
    return $ bg >> fg
