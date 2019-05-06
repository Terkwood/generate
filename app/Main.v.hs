module Main where

import Control.Monad.Extra
import Control.Monad.Reader
import Data.Colour.SRGB
import Data.List
import Data.Maybe
import Data.Ord
import Data.RVar
import Data.Random.Distribution.Normal
import Data.Random.Distribution.Uniform
import Data.Tuple.HT
import qualified Data.Vector as V
import Graphics.Rendering.Cairo as Cairo
import Linear
import Math.Noise
import Math.Spline
import Math.Spline.BSpline
import System.IO.Unsafe

import Algo.CirclePack
import qualified Algo.QuadTree as Q
import Colour
import Coord
import Generate
import Geom
import Geom.Circle
import Geom.Line
import Geom.Rect
import Patterns.Grid
import Patterns.Maze
import Patterns.Sampling
import Patterns.THColors

ramp :: Int -> V.Vector Double
ramp n = V.generate n $ \i -> fromIntegral i / fromIntegral n

arcRamp :: Int -> V.Vector Double
arcRamp n = V.map (\v -> sin $ v * pi) $ ramp n

extendedRamp :: Int -> Int -> V.Vector Double
extendedRamp degree n = V.concat [start, ramp n, end]
  where
    start = V.generate degree $ const 0.0
    end = V.generate degree $ const 1.0

clamp :: Double -> Double
clamp t =
  if t > 1.0
    then 1.0
    else if t < 0.0
           then 0.0
           else t

data Spline2d = Spline2d
  { xSpline :: BSpline V.Vector Double
  , ySpline :: BSpline V.Vector Double
  }

mkSpline2d :: Int -> Line -> Spline2d
mkSpline2d degree line = Spline2d xSpline ySpline
  where
    verts = toVertices line
    n = V.length verts
    knots = mkKnots $ V.toList $ extendedRamp degree n
    xSpline = bSpline knots $ V.map (\(V2 x _) -> x) verts
    ySpline = bSpline knots $ V.map (\(V2 _ y) -> y) verts

sample :: Spline2d -> Double -> V2 Double
sample (Spline2d xSpline ySpline) t = V2 x y
  where
    x = evalBSpline xSpline t
    y = evalBSpline ySpline t

wigglePoint :: Double -> V2 Double -> Generate (V2 Double)
wigglePoint strength p = do
  offset <- sampleRVar $ normal 0 $ sqrt strength
  theta <- sampleRVar $ uniform 0 (2 * pi)
  return $ circumPoint p theta offset

warp :: Line -> Double -> Generate Line
warp line strength =
  (V.sequence $ V.map (wigglePoint strength) $ toVertices line) >>=
  return . fromJust . mkLine

drawCircle :: Circle -> Render ()
drawCircle (Circle (V2 x y) r) = do
  arc x y r 0 (2 * pi)
  fill

drawSpline ::
     Int
  -> (Double -> V2 Double -> Generate (Render ()))
  -> Spline2d
  -> Generate (Render ())
drawSpline n dotter spline =
  let samplePoints = ramp n
      samples = V.map (sample spline) samplePoints
   in do dots <-
           V.sequence $ V.map (uncurry dotter) $ V.zip samplePoints samples
         return $ V.foldr1 (>>) dots

alphaMatte :: Render () -> Render () -> Render ()
alphaMatte matte src = do
  pushGroup
  src
  popGroupToSource
  pushGroup
  matte
  withGroupPattern mask

data Sweep = Sweep
  { sweepPos :: V2 Double
  , sweepAmplitude :: Double
  , sweepFrequency :: Double
  , sweepDots :: Int
  , sweepVariance :: Double
  , sweepHeight :: Double
  }

--drawDot :: V2 Double -> Render ()
--drawDot p = drawCircle $ Circle p 0.5
drawDot :: V2 Double -> Generate (Render ())
drawDot (V2 x y) = do
  World {..} <- asks world
  return $ do
    setLineWidth $ 0.4
    moveTo x y
    lineTo (x + (1.0 / scaleFactor)) y
    closePath
    stroke

drawSweep :: Sweep -> Generate (Render ())
drawSweep (Sweep (V2 x y) amp freq dots _ height) = do
  let phase = freq / fromIntegral dots
  let ys =
        V.generate dots $ \i ->
          y + (negate 1) * abs (amp * sin (fromIntegral i / height * freq))
  let ps = V.map (V2 x) ys
  dotDraws <- V.sequence $ V.map (drawDot) ps
  return $ V.foldr1 (>>) dotDraws

stepSweep :: Sweep -> Generate (Maybe Sweep)
stepSweep sweep@(Sweep (V2 x y) _ freq _ variance _) = do
  World {..} <- asks world
  let freqVariance = variance / scaleFactor
  freqDelta <- (sampleRVar $ uniform (negate freqVariance) freqVariance)
  let freq' = 0.1 * (clamp $ (freq + freqDelta) / 0.1)
  if x > width
    then return Nothing
    else return $
         Just
           sweep
             {sweepPos = V2 (x + 1.0 / scaleFactor) y, sweepFrequency = freq'}

sweep :: Double -> Double -> Double -> Generate (Render ())
sweep amp variance height = do
  World {..} <- asks world
  let xs = floor $ width * scaleFactor
  sweeps <-
    iterateMaybeM stepSweep (Sweep (V2 0 height) amp 0.001 400 variance height)
  sweepDraws <- sequence $ map (drawSweep) $ take xs sweeps
  c <- fgColour
  return $ do
    setColour (c, 0.2 :: Double)
    foldr1 (>>) sweepDraws

solidLayer :: RGB Double -> Generate (Render ())
solidLayer colour = do
  World {..} <- asks world
  frame <- fullFrame
  return $ do
    setColour colour
    rectangle 0 0 width height
    fill

scene :: Generate (Render ())
scene = do
  World {..} <- asks world
  frame <- fullFrame
  bg <- solidLayer bgColour
  hillCount <- sampleRVar $ uniform 4 30
  heights <-
    V.sequence $ V.generate hillCount $ const $ sampleRVar $ uniform 100 40000
  amplitudes <-
    V.sequence $ V.map (const $ sampleRVar $ uniform 10000.0 800000.0) heights
  frequencies <-
    V.sequence $ V.map (const $ sampleRVar $ uniform 0.000002 0.000005) heights
  brooms <-
    V.sequence $
    V.map (\(height, amp, freq) -> sweep amp freq height) $
    V.zip3 heights amplitudes frequencies
  return $ do
    setAntialias AntialiasBest
    bg
    foldr1 (>>) brooms
    return ()

fgColour :: Generate (RGB Double)
fgColour = do
  let palette =
        V.map (hexcolour) $
        V.fromList $ ["0C1B20", "065275", "328197", "25445D", "77D0E4"]
  i <- sampleRVar $ uniform 0 (V.length palette - 1)
  return $ palette V.! i

bgColour :: RGB Double
bgColour = hexcolour "F6F9F5" --"ffffff"

main :: IO ()
main = runInvocation scene
