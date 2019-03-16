module Main where

import Control.Monad.Extra
import Control.Monad.Reader
import Data.Colour.SRGB
import Data.List
import Data.Maybe
import Data.RVar
import Data.Random.Distribution.Normal
import Data.Random.Distribution.Uniform
import qualified Data.Vector as V
import Graphics.Rendering.Cairo as Cairo
import Linear
import Math.Noise
import System.IO.Unsafe

import Colour
import Coord
import Generate
import Geom.Circle
import Geom.Rect
import Patterns.Grid

scene :: Generate (Render ())
scene = do
  World {..} <- asks world
  xOff <- sampleRVar $ uniform (width / 5) (width / 5 * 4)
  let start = V2 xOff height
  result <- walk (step) start 10000
  let path = line result
  let shapes = madePaths result
  return $ do
    setColour bgColour
    rectangle 0 0 width height
    fill
    setColour $ hexcolour "E7F5F5"
    linePath path
    foldr (>>) (pure ()) $ map ((>> fill) . linePath) shapes
    stroke

data Walk = Walk
  { stride :: Double
  , noiseScale :: Double
  , turn :: Maybe (Int, Int -> Double)
  , targetTheta :: Double
  , line :: Line
  , lastIntersection :: Int
  , madePaths :: [Line]
  }

mkWalk :: V2 Double -> Generate Walk
mkWalk start = do
  World {..} <- asks world
  stride <- sampleRVar $ uniform 1 3
  denom <- sampleRVar $ uniform 1 5 >>= return . (width /)
  return $ Walk stride (1 / denom) Nothing 1 (Line $ V.fromList [start]) 0 []

checkIntersections :: Walk -> V2 Double -> Walk
checkIntersections w@(Walk {..}) p =
  case intersection of
    Just i ->
      w
        { lastIntersection = lastIntersection + i
        , madePaths = (Line $ V.slice lastIntersection i verts) : madePaths
        }
    Nothing -> w
  where
    Line verts = line
    candidates =
      V.slice lastIntersection (V.length verts - lastIntersection) verts
    intersection = V.findIndex (\op -> distance op p < stride) candidates

addTurn :: Walk -> Int -> Double -> Walk
addTurn w steps degree = w {turn = Just (steps, f)}
  where
    x i = (fromIntegral i / fromIntegral steps) * pi
    f i = degree * sin (x i)

walkTurn :: Walk -> (Walk, Maybe Double)
walkTurn w@(Walk {..}) =
  case turn of
    Nothing -> (w, Nothing)
    Just (remaining, f) ->
      if remaining >= 1
        then (w {turn = Just (remaining - 1, f)}, Just $ f remaining)
        else (w {turn = Nothing}, Nothing)

step :: Walk -> Generate Walk
step w@(Walk {..}) = do
  World {..} <- asks world
  let Line verts = line
  let p@(V2 x y) = V.last verts
  frame <- fullFrame
  noise <-
    noiseSample (V3 (x * noiseScale) (y * noiseScale) 1.1) >>= return . abs
  shouldGenTurn :: Double <- sampleRVar $ uniform 0 1
  turnSteps <- sampleRVar $ uniform 80 300
  turnDegree <- sampleRVar $ uniform 0 (2 * pi)
  let (w', turn) = walkTurn w
  let w'' =
        if not (isJust turn) && shouldGenTurn < 0.6
          then addTurn w' turnSteps turnDegree
          else w'
  let phase = noise * 2 * pi
  let phase' =
        case turn of
          Just t -> phase + t
          Nothing -> phase
  let next = circumPoint p (phase' * targetTheta) stride
  let newTarget =
        if not $ withinRect frame next
          then targetTheta * (negate 1)
          else targetTheta
  return $
    (checkIntersections w'' next)
      {targetTheta = newTarget, line = Line $ V.snoc verts next}

data Line =
  Line (V.Vector (V2 Double))

linePath :: Line -> Render ()
linePath (Line points) = do
  let (V2 x y) = V.head points
  moveTo x y
  V.foldr (>>) (pure ()) $ V.map (\(V2 x y) -> lineTo x y) $ V.tail points

walk :: (Walk -> Generate Walk) -> V2 Double -> Int -> Generate Walk
walk stepper start steps = do
  walk <- mkWalk start
  let stepper' (walk, remaining) =
        if remaining < 1
          then pure Nothing
          else do
            walk' <- stepper walk
            return $ Just (walk', remaining - 1)
  iterateMaybeM stepper' (walk, steps) >>= return . fst . (!! (steps - 1))

bgColour :: RGB Double
bgColour = hexcolour "8ACEDA"

main :: IO ()
main = runInvocation scene
