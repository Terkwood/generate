module Main where

import Control.Monad.Reader
import Data.Colour.SRGB
import Data.List
import Data.RVar
import Data.Random.Distribution.Normal
import Data.Random.Distribution.Uniform
import qualified Data.Vector as V
import Graphics.Rendering.Cairo as Cairo
import Linear
import Math.Noise
import System.IO.Unsafe

import Colour
import Generate
import Patterns.Grid

data State = State
  { batches :: V.Vector Batch
  }

initState :: Generate State
initState = do
  flowCount <- sampleRVar $ uniform 1 3
  flows <-
    V.sequence $
    V.generate flowCount $ const (randomPoint >>= return . (* 1000000))
  batchCount <- sampleRVar $ uniform 250 500
  batchOrigins <- V.sequence $ V.generate batchCount $ const $ randomPoint
  batches <-
    V.sequence $
    V.generate batchCount $ \i ->
      mkBatch (batchOrigins V.! i) (flows V.! (i `mod` V.length flows))
  return $ State batches

realize :: State -> Generate (Render ())
realize (State batches) = do
  World {..} <- asks world
  return $ do
    setLineCap LineCapRound
    setColour bgColour
    rectangle 0 0 width height
    fill
    V.sequence $ V.map (renderBatch) batches
    return ()

step :: State -> Generate State
step (State batches) = do
  batches' <- V.sequence $ V.map (stepBatch) batches
  return $ State $ batches'

stepBatch :: Batch -> Generate Batch
stepBatch batch@(Batch strokes _) = do
  strokes' <- V.sequence $ V.map (stepStroke) strokes
  return $ batch {strokes = strokes'}

stepStroke :: Stroke -> Generate Stroke
stepStroke (Stroke lines) = do
  delta <- sampleRVar $ uniform 0 1
  let (Line points) = V.head lines
  let origin = V.head points
  theta <- scaledNoiseSample origin
  return $ Stroke $ V.map (stepLine origin (theta + (pi / 2)) delta) lines

stepLine :: V2 Double -> Double -> Double -> Line -> Line
stepLine pivot theta delta (Line points) = Line $ V.map stepPoint points
  where
    stepPoint :: V2 Double -> V2 Double
    stepPoint p = circumPoint p theta delta

scaledNoiseSample :: V2 Double -> Generate Double
scaledNoiseSample (V2 x y) = do
  World {..} <- asks world
  noiseSample (V3 (x / width * 1.2) (y / height * 1.2) 0)

mkBatch :: V2 Double -> V2 Double -> Generate Batch
mkBatch center offset = do
  strokeCount :: Int <-
    (sampleRVar $ normal 220 30 :: Generate Double) >>= return . floor
  radius <- sampleRVar $ normal 100 20
  strokes <-
    V.sequence $
    V.generate strokeCount $
    const (randomPointAbout center radius >>= mkStroke offset)
  colour <- samplePalette line2Palette
  return $ Batch strokes colour

choosePalette :: Generate (V.Vector String)
choosePalette = do
  let palettes = V.fromList [linePalette, line2Palette]
  index <- sampleRVar $ uniform 0 100
  return $ palettes V.! (index `mod` V.length palettes)

line2Palette :: V.Vector String
line2Palette =
  V.fromList
    ["0E6E79", "E9BB52", "1D5076", "0EC6B7", "FBF9F4", "DD985F", "D2344A"]

correctTheta :: V2 Double -> V2 Double -> Double -> Double
correctTheta center p theta = do
  let dist = distance p center
  let p' = circumPoint p theta 1
  let dist' = distance p' center
  if dist' > dist
    then theta + pi
    else theta

data Line =
  Line (V.Vector (V2 Double))

data Stroke =
  Stroke (V.Vector Line)

data Batch = Batch
  { strokes :: V.Vector Stroke
  , colour :: RGB Double
  }

renderBatch :: Batch -> Render ()
renderBatch (Batch strokes colour) = do
  let width = 2.2
  V.sequence $ V.map (drawStroke (width * 1.4) (hexcolour "#000000")) strokes
  V.sequence $ V.map (drawStroke width colour) strokes
  return ()

randomPointAbout :: V2 Double -> Double -> Generate (V2 Double)
randomPointAbout center radius = do
  theta <- sampleRVar $ uniform 0 (2 * pi)
  distance <- sampleRVar $ normal 0 1 >>= return . abs
  return $ circumPoint center theta (distance * radius)

randomPoint :: Generate (V2 Double)
randomPoint = do
  World {..} <- asks world
  let border = 0.2
  x <- sampleRVar $ uniform (0 - border) (1 + border)
  y <- sampleRVar $ uniform (0 - border) (1 + border)
  return $ V2 (x * width) (y * height)

drawStroke :: Double -> RGB Double -> Stroke -> Render ()
drawStroke width colour (Stroke lines) = do
  setColour colour
  setLineWidth width
  V.sequence $ V.map ((>> stroke) . drawLine) lines
  return ()

drawLine :: Line -> Render ()
drawLine (Line points) = do
  newPath
  let (V2 sx sy) = V.head points
  moveTo sx sy
  sequence $ V.map (\(V2 x y) -> lineTo x y) $ V.tail points
  return ()

mkStroke :: V2 Double -> V2 Double -> Generate Stroke
mkStroke offset origin = do
  World {..} <- asks world
  frame <- asks frame >>= return . fromIntegral
  theta <- scaledNoiseSample (origin + offset) >>= return . (* 2) . (* pi)
  strokeCount <- sampleRVar $ uniform 4 6
  let offsetF i = circumPoint origin (theta + (pi / 2)) (i * 2)
  let batchOrigins =
        V.generate strokeCount $ \i -> origin + offsetF (fromIntegral i)
  amplitude <- sampleRVar $ uniform 0 pi
  frequency <- sampleRVar $ uniform (0.0001) (0.0005)
  let sin' x = amplitude * sin (x * frequency)
  pointCount :: Double <- sampleRVar $ normal 20 1
  let mkLine' = mkLine (abs $ floor pointCount) 1 theta sin'
  lines <- V.sequence $ V.map (mkLine') batchOrigins
  return $ Stroke lines

mkLine ::
     Int -> Double -> Double -> (Double -> Double) -> V2 Double -> Generate Line
mkLine points crawl theta distorter origin = do
  let pointGen = iterate nextPoint $ pure []
  pointGen !! points >>= return . Line . V.fromList
  where
    nextPoint :: Generate [V2 Double] -> Generate [V2 Double]
    nextPoint ps = do
      ps_ <- ps
      let p@(V2 x y) =
            if null ps_
              then origin
              else head ps_
      distortionAmplitude <- sampleRVar $ uniform 0 0.4
      distortionX <-
        sampleRVar $ uniform (0 - distortionAmplitude) distortionAmplitude
      distortionY <-
        sampleRVar $ uniform (0 - distortionAmplitude) distortionAmplitude
      let distortion = V2 distortionX distortionY
      let dist = distance p origin
      let p' = circumPoint p (theta + distorter x) crawl
      return $ (p' + distortion) : ps_

thetaOf :: V2 Double -> V2 Double -> Double
thetaOf (V2 cx cy) (V2 x y) = atan2 (y - cy) (x - cx)

circumPoint :: V2 Double -> Double -> Double -> V2 Double
circumPoint (V2 x y) theta r = (V2 (x + r * cos theta) (y + r * sin theta))

borderColour :: RGB Double
borderColour = hexcolour "F7F0DE"

bgColour :: RGB Double
bgColour = hexcolour "F9E4AD" --"F7F0DE"

linePalette :: V.Vector String
linePalette = V.fromList ["53363B", "A7707A", "B9B886", "556A4A"] --V.fromList ["26343C", "313744", "83B1AB", "C5B08C"]

samplePalette :: V.Vector String -> Generate (RGB Double)
samplePalette colours = do
  index <- sampleRVar $ uniform 0 $ V.length colours - 1
  return $ hexcolour $ colours V.! index

colourWithPalette :: V.Vector String -> Render () -> Generate (Render ())
colourWithPalette palette path = do
  colour <- samplePalette palette
  return $ do
    path
    setColour colour
    fill

main :: IO ()
main = runStatefulInvocation initState realize step
