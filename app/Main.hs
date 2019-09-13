module Main where

import qualified Data.Vector as V
import Linear hiding (rotate)
import System.IO.Unsafe

import Generate
import qualified Generate.Algo.CirclePack as P
import qualified Generate.Algo.QuadTree as Q
import qualified Generate.Algo.Vec as V
import Generate.Colour.SimplePalette
import Generate.Colour.THColours
import Generate.Geom.Frame
import Generate.Patterns.Grid
import Generate.Patterns.Maze
import Generate.Patterns.RecursiveSplit
import Generate.Patterns.Sampling
import Generate.Patterns.Water
import Petal
import qualified Streaming as S
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
    , gurken
    , mote
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

data State =
  State
    { palette :: SimplePalette
    , pathOrigin :: V2 Double
    }

start :: Generate State
start = do
  palette <- mkPalette
  pathOrigin <- centerPoint
  return $ State {palette = palette, pathOrigin = pathOrigin}

data Step =
  Step
    { stepIdx :: Int
    , theta :: Double
    , position :: V2 Double
    , turnTheta :: Double
    }

data Walker =
  Walker
    { agility :: Generate Double
    , step :: Double
    }

gaussianWalker :: Double -> Walker
gaussianWalker strength =
  Walker {agility = sampleRVar $ normal 0 strength, step = 0.4}

walk :: Walker -> V2 Double -> Stream Step
walk walker origin =
  let first = do
        theta <- sampleRVar $ uniform 0 (2 * pi)
        return $ Step 0 theta origin pi
   in S.iterateM (_step walker) first

_step :: Walker -> Step -> Generate Step
_step (Walker {..}) (Step {..}) = do
  turn <- agility
  let (turnTheta', turn') =
        if theta `mod'` (2 * pi) > turnTheta
          then ((turnTheta + pi) `mod'` (2 * pi), turn)
          else (turnTheta, turn' * 2)
  let theta' = theta + turn
  let next = circumPoint position theta step
  return $ Step (stepIdx + 1) theta' next turnTheta'

intersectionPolies :: Stream Step -> Generate [Line]
intersectionPolies steps = do
  steps <- S.toList_ steps >>= return . V.fromList
  frame <- fullFrame
  let (PolySearch results) = V.foldr (search steps) (PolySearch []) steps
  return $
    mapMaybe
      (\(start, end) -> mkLine $ V.map position $ V.slice start end steps)
      results

data Shape =
  Shape
    { line :: Line
    }

instance Drawable Shape where
  draw (Shape line) = do
    setSourceRGB 0 0 0
    draw line
    fill

data PolySearch =
  PolySearch
    { found :: [(Int, Int)]
    }

search :: V.Vector Step -> Step -> PolySearch -> PolySearch
search vs s@(Step {..}) (PolySearch {..}) =
  PolySearch $
  case V.find ((< 1) . (distance position) . (\(Step {..}) -> position)) vs of
    Just (Step i _ _ _) -> (i, stepIdx - i) : found
    Nothing -> found

data Dot =
  Dot
    { center :: V2 Double
    }

instance Element Dot where
  realize (Dot c) = do
    return $ do
      setSourceRGB 0 0 0
      draw $ Circle c 1
      fill

sketch :: State -> Stream (Render ())
sketch state@(State {..}) =
  streamGenerates [background palette] >> unfoldGenerates shapes
  where
    shapes :: Generate [Render ()] = do
      polies :: [Line] <- intersectionPolies $ S.take 10000 steps
      return $ map (draw . Shape) polies
    steps = walk (gaussianWalker 0.1) pathOrigin

main :: IO ()
main = do
  runStatefulInvocation start sketch return
