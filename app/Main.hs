module Main where

import qualified Data.Vector as V
import Linear hiding (rotate)

import Control.Exception
import Debug.Trace as D
import Generate
import Generate.Patterns.Grid
import qualified Streaming as S
import qualified Streaming.Prelude as S

palette :: SimplePalette
palette = mkSimplePalette "b5fffa" ["652141", "F14111", "EE7820", "F3BF0F"]

data LineCfg = LineCfg
  { lineLength :: Int
  , gapLength :: Int
  }

cfgLength :: LineCfg -> WalkHead -> Int
cfgLength cfg Line = lineLength cfg
cfgLength cfg Gap = gapLength cfg

data Walk = Walk
  { head :: WalkHead
  , headLength :: Int
  , walkPos :: V2 Double
  }

data WalkHead
  = Line
  | Gap

altHead Line = Gap
altHead Gap = Line

step :: LineCfg -> Walk -> Generate Walk
step cfg (Walk head headLength (V2 x y)) = do
  terminalLength :: Double <- sampleRVar $ normal lengthDouble 20
  let head' =
        if headLength > floor terminalLength
          then altHead head
          else head
  return $ Walk head' (cfgLength cfg head') nextPos
  where
    nextPos = V2 x (y + 1)
    lengthDouble = fromIntegral $ cfgLength cfg head

dots :: Stream (Render ())
dots = do
  let points = unfoldGenerates $ grid def
  let circles = S.map (\p -> Circle p 10) points
  S.mapM
    (\c@(Circle p@(V2 x y) r) -> do
       colour <- fgColour palette
       t <- asks time
       noise <- noiseSample (V3 x y $ t)
       let center = noiseShift noise 10 p
       return $ do
         setColour colour
         draw $ Circle center r
         fill)
    circles

main :: IO ()
main = do
  runSketch $ do
    return $ streamGenerates [background $ bgColour palette] >> dots
