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
import Generate.Raster.Sand

import PetalStroke

mkPalette :: Generate SimplePalette
mkPalette = randElem $ V.fromList [jhoto, castle, metroid, gurken, mondrian]

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
    setLineWidth 0.5
    foldr1 (>>) $
      map
        (\(p, c) -> do
           setColour c
           draw p
           stroke) $
      zip paths colours

bandBox :: SimplePalette -> Rect -> Generate (Render ())
bandBox palette box = do
  c <- fgColour palette
  let bounds = intoBounds $ UniformBands 3
  let box' = scaleFromCenter 0.7 box
  let bands = [box'] -- bandsOnAxis X bounds box'
  scales <- sequence $ map (const $ sampleRVar $ normal 1 0.3) bands
  let bands' = map (uncurry scaleFromCenter) $ zip scales bands
  let polies = map (fromJust . mkLine) bands'
  let splotches = map (\p -> mkSplotch p c) polies
  wigglePower <- sampleRVar $ normal 1 0.4
  let wiggler =
        Wiggler $ \p@(V2 x y) -> do
          n <- noiseSample $ V3 x y 1.2
          let w = radialWiggler $ n * wigglePower
          wiggle w p
  splotches' <- sequence $ map (waterColour wiggler def) splotches
  return $ foldr (>>) (pure ()) $ map ((>> fill) . draw) $ concat splotches'

sandCfg = SandCfg {attempts = 500, mkElement = \p -> pure $ Circle p 0.2}

scene :: SimplePalette -> Generate (Render ())
scene palette = do
  root <- centerPoint
  frame <- fullFrame
  let box = scaleFromCenter 0.8 frame
  colour <- fgColour palette
  let predicate :: Generate Bool =
        sampleRVar (uniform (negate 1) 1) >>= \(v :: Double) -> return $ v > 0
  boxes <- recursiveSplit def frame -- >>= fairFilter predicate
  let boxes' = map (scaleFromCenter 0.8) boxes
  boxSands <- concatMapM (sandPaint sandCfg) boxes'
  bandBoxes <- sequence $ map (bandBox palette) boxes
  boxDraws <-
    sequence $
    map
      (\b -> do
         colour <- fgColour palette
         return $ do
           setColour (colour, 0.5 :: Double)
           draw b
           fill)
      boxSands
  return $ do
    setColour colour
    setLineWidth 1
    foldr (>>) (pure ()) boxDraws

main :: IO ()
main = do
  putStrLn $ show $ intoBounds $ UniformBands 3
  runInvocation $ do
    palette <- mkPalette
    bg <- background palette
    fg <- scene palette
    return $ bg >> fg
