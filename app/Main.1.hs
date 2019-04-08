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

sandStroke :: THColors -> Line -> Generate (Render ())
sandStroke thcol line = do
  let count = 1
  let strength = 5
  let strengths = V.map (* strength) $ ramp count
  let dots = 15000
  let dotSize = 0.3
  let dotSpread = dotSize * 20.3
  let vertices = toVertices line
  let dotter samplePoint point = do
        radius <- sampleRVar $ normal 0 dotSpread >>= return . abs
        let sampleSpace = Circle point radius
        dotPoint <- spatialSample sampleSpace
        colour <- assignTHColor thcol dotPoint
        return $ do
          setColour colour
          drawCircle $ Circle dotPoint dotSize
  lines <- V.sequence $ V.map (warp line) strengths
  let splines = V.map (mkSpline2d 5) lines
  draws <- V.sequence $ V.map (drawSpline dots dotter) splines
  return $ V.foldr1 (>>) draws

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
  rows <- sampleRVar $ uniform 2 20
  cols <- sampleRVar $ uniform 2 20
  lines <- maze gridCfgDefault {rows = rows, cols = cols}
  thcol <-
    mkTHColors $
    V.map (hexcolour) $ V.fromList ["987FA1", "5E527D", "20263D", "6A6169"]
  draws <- sequence $ map (sandStroke thcol . subdivideN 3) lines
  return $ do
    setAntialias AntialiasBest
    bg
    foldr1 (>>) draws
    return ()

fgColour :: Generate (RGB Double)
fgColour = do
  let palette = V.map (hexcolour) $ V.fromList $ []
  i <- sampleRVar $ uniform 0 (V.length palette - 1)
  return $ palette V.! i

bgColour :: RGB Double
bgColour = hexcolour "ACA2C1" --"ffffff"

main :: IO ()
main = runInvocation scene
