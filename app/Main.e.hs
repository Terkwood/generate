module Main where

import Control.Monad.Extra
import Control.Monad.Reader
import Data.Colour.SRGB
import Data.List
import Data.Ord
import Data.RVar
import Data.Random.Distribution.Normal
import Data.Random.Distribution.Uniform
import qualified Data.Vector as V
import Graphics.Rendering.Cairo as Cairo
import Linear
import Math.Noise
import System.IO.Unsafe

import qualified Algo.QuadTree as Q
import Colour
import Coord
import Generate
import Patterns.Grid

data Circle = Circle
  { center :: V2 Double
  , radius :: Double
  }

overlap :: Circle -> Circle -> Double
overlap (Circle c1 r1) (Circle c2 r2) = (r1 + r2) - distance c1 c2

randomCircle :: Double -> Generate (V2 Double) -> Generate Circle
randomCircle maxSize pointGen = do
  radius <- sampleRVar $ uniform 3 maxSize
  center <- pointGen
  return $ Circle center radius

drawCircle :: Circle -> Generate (Render ())
drawCircle (Circle (V2 x y) r) = do
  colour <- samplePalette line2Palette
  return $ do
    arc x y r 0 (2 * pi)
    setColour colour
    fill

data CircleSearch = CircleSearch
  { foundCircles :: [Circle]
  , remaining :: Int
  }

mkSearch :: Int -> CircleSearch
mkSearch attempts = CircleSearch [] attempts

attemptSearch ::
     Generate Circle -> CircleSearch -> Generate (Maybe CircleSearch)
attemptSearch generator s@(CircleSearch cs remaining) =
  if remaining < 1
    then pure Nothing
    else do
      let remaining' = remaining - 1
      candidate <- generator
      if any ((> 0) . (overlap candidate)) cs
        then return $ Just $ CircleSearch cs remaining'
        else return $ Just $ CircleSearch (candidate : cs) remaining'

findCircles :: Int -> Generate (V.Vector Circle)
findCircles attempts = do
  maxRadius <- sampleRVar $ uniform 30 100
  let centers = do
        xOff <- sampleRVar $ normal 0 20
        yOff <- sampleRVar $ normal 0 20
        (V2 x y) <- centerPoint
        return $ (V2 (x + xOff) (y + yOff))
  iterations <-
    iterateMaybeM
      (attemptSearch $ randomCircle maxRadius centers)
      (mkSearch attempts)
  let CircleSearch circles _ = iterations !! (attempts - 1)
  return $ V.fromList circles

estimateCircle :: Int -> Circle -> V.Vector (V2 Double)
estimateCircle res (Circle c r) =
  V.generate res $ \i ->
    let theta = (fromIntegral i / fromIntegral res) * 2 * pi
     in circumPoint c theta r

moveToward :: V2 Double -> V2 Double -> Double -> V2 Double
moveToward c p d =
  let theta = thetaOf c p
      r = distance c p
   in circumPoint c theta (r - d)

drapeOver ::
     Double
  -> V2 Double
  -> V.Vector Circle
  -> V.Vector (V2 Double)
  -> V.Vector (V2 Double)
drapeOver margin center circles points =
  if done
    then points'
    else drapeOver margin center circles points'
  where
    intersectAny p =
      let c = Circle p 1
       in V.any ((> (0 - margin)) . (overlap c)) circles
    update p =
      let step =
            V.minimum $ V.map ((* (negate 1)) . overlap (Circle p 1)) circles
          step' = max (step / 3 * 2) 3
          p' = moveToward center p step'
       in if intersectAny p' || distance p' center < 5
            then (False, p)
            else (True, p')
    updates = V.map (update) points
    points' = V.map (snd) updates
    done = not $ V.any (fst) updates

drawLine :: V.Vector (V2 Double) -> Generate (Render ())
drawLine points = do
  colour <- samplePalette line2Palette
  width <- sampleRVar $ uniform 0.2 4
  dashMode :: Int <- sampleRVar $ uniform 0 1
  dashes <-
    if dashMode == 1
      then sequence $ map (const $ sampleRVar $ uniform 1 40) [0 .. 10]
      else pure []
  let (V2 x y) = V.head points
  let steps :: V.Vector (Render ()) =
        V.map (\(V2 x y) -> lineTo x y) $ V.tail points
  return $ do
    setColour colour
    setLineWidth width
    setDash dashes 0
    moveTo x y
    V.foldr1 (>>) steps
    closePath
    stroke

drawPath :: RGB Double -> V.Vector (V2 Double) -> Generate (Render ())
drawPath colour points = do
  let (V2 x y) = V.head points
  let steps :: V.Vector (Render ()) =
        V.map (\(V2 x y) -> lineTo x y) $ V.tail points
  return $ do
    setColour colour
    moveTo x y
    V.foldr1 (>>) steps
    closePath
    fill
    -- Moves every point halfway to the midpoint of its neighbors

smooth :: V.Vector (V2 Double) -> V.Vector (V2 Double)
smooth points = V.map (uncurry midpoint) $ V.zip ms points
  where
    s1 = V.cons (V.last points) points
    s2 = V.snoc points $ V.head points
    s = V.zip s1 s2
    ms = V.map (uncurry midpoint) s

echoLine ::
     Int -> V.Vector (V2 Double) -> Generate (V.Vector (V.Vector (V2 Double)))
echoLine echos line = do
  c <- centerPoint
  throws <-
    V.sequence $
    V.generate echos $ const $ (sampleRVar $ normal 0 40 >>= return . abs)
  return $
    V.map (\throw -> V.map (\p -> moveToward c p (negate throw)) line) throws

outlineCluster :: Int -> V.Vector Circle -> Generate (V.Vector (V2 Double))
outlineCluster res circles = do
  c <- centerPoint
  margin <- sampleRVar $ uniform 5 10
  let drape = estimateCircle res $ Circle c 800
  let raw = drapeOver margin c circles drape
  let smoothed = iterate (smooth) raw
  return $ smoothed !! 100

bg :: V.Vector Circle -> Generate (Render ())
bg circles = do
  World {..} <- asks world
  r <- sampleRVar $ uniform 5 50
  c <- sampleRVar $ uniform 5 50
  center <- centerPoint
  points <- grid gridCfgDefault {rows = r, cols = c}
  scales <-
    sequence $
    map
      (\(V2 x y) -> noiseSample (V3 (x / width) (y / height) 0) >>= return . abs)
      points
  let dots = map (\(s, p) -> Circle p $ (s * 0.4) + 1) $ zip scales points
  dotDraws <- sequence $ map (drawCircle) dots
  return $ do
    setColour bgColour
    rectangle 0 0 width height
    fill
    foldr1 (>>) dotDraws
    setColour (bgColour, 0.4 :: Double)
    rectangle 0 0 width height
    fill

scene :: Generate (Render ())
scene = do
  circles <- findCircles 500
  resolution <- sampleRVar $ uniform 200 2000
  outline <- outlineCluster resolution circles
  echoCount <- sampleRVar $ uniform 1 30
  outlines <- echoLine echoCount outline
  outlineDraws <- V.sequence $ V.map (drawLine) outlines
  circleDraws <- V.sequence $ V.map (drawCircle) circles
  bgDraw <- bg circles
  c <- centerPoint
  let outlineMargin = V.map (\p -> moveToward c p $ negate 80) outline
  marginDraw <- drawPath bgColour $ (iterate (smooth) outlineMargin) !! 100
  return $ do
    bgDraw
    marginDraw
    V.foldr1 (>>) circleDraws
    V.foldr1 (>>) outlineDraws
    return ()

line2Palette :: V.Vector String
line2Palette =
  V.fromList
    ["0E6E79", "E9BB52", "1D5076", "0EC6B7", "FBF9F4", "DD985F", "D2344A"]

randomPointAbout :: V2 Double -> Double -> Generate (V2 Double)
randomPointAbout center radius = do
  theta <- sampleRVar $ uniform 0 (2 * pi)
  distance <- sampleRVar $ normal 0 1 >>= return . abs
  return $ circumPoint center theta (distance * radius)

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

main :: IO ()
main = runInvocation scene
