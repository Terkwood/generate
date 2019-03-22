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
import qualified Data.Vector as V
import Graphics.Rendering.Cairo as Cairo
import Linear
import Math.Noise
import System.IO.Unsafe

import qualified Algo.QuadTree as Q
import Colour
import Coord
import Generate
import Geom.Circle
import Geom.Rect
import Patterns.Grid

data Circle =
  Circle (V2 Double)
         Double

overlap :: Circle -> Circle -> Double
overlap (Circle c1 r1) (Circle c2 r2) = r1 + r2 - distance c1 c2

data CircleSearch = CircleSearch
  { searchTree :: Q.QuadTree Circle
  , foundCircles :: [Circle]
  , remainingAttempts :: Int
  , searchFrame :: Rect
  }

mkSearch :: Int -> Rect -> CircleSearch
mkSearch n frame =
  CircleSearch (Q.new (representativeNodeUpdater) frame) [] n frame
  where
    leafRadius :: Q.Leaf Circle -> Double
    leafRadius = (\(Circle _ r) -> r) . Q.leafTag
    representativeNodeUpdater ::
         Maybe (Q.Leaf Circle) -> Q.Leaf Circle -> Maybe (Q.Leaf Circle)
    representativeNodeUpdater current new =
      case current of
        Just current -> Just $ maximumBy (comparing leafRadius) [new, current]
        Nothing -> Just new

randomCircle :: Rect -> Generate Circle
randomCircle frame = do
  radius <- sampleRVar $ normal 5 3 >>= return . (+ 1) . abs
  center <- randomPointIn frame
  return $ Circle center radius

heuristic :: Q.Heuristic Circle
heuristic =
  Q.Heuristic
    { heuristicDistance = \(Q.Leaf _ c1) (Q.Leaf _ c2) -> negate $ overlap c1 c2
    , heuristicFilter =
        \(Q.Leaf _ best) (Q.Leaf p search@(Circle _ sr)) (Q.Quad _ reg rep _) ->
          case rep of
            Just (Q.Leaf _ (Circle _ cr)) ->
              let distanceToRegion = distanceToRect reg p
                  distanceToClosestPossible = distanceToRegion - cr - sr
               in distanceToClosestPossible <
                  ((negate $ overlap best search) :: Double)
            Nothing -> False
    }

valid :: Q.QuadTree Circle -> Circle -> Bool
valid tree c@(Circle center _) =
  let leaf = Q.nearest heuristic (Q.Leaf center c) tree
   in case leaf of
        Just (Q.Leaf _ nearestCircle) -> not $ overlap c nearestCircle > 0
        Nothing -> True

search :: CircleSearch -> Generate (Maybe CircleSearch)
search (CircleSearch _ _ 0 _) = return Nothing
search s@(CircleSearch tree circles remaining frame) = do
  let remaining' = remaining - 1
  candidate@(Circle center _) <- randomCircle frame
  if valid tree candidate
    then return $
         Just $
         CircleSearch
           (snd $ Q.insert (Q.Leaf center candidate) tree)
           (candidate : circles)
           remaining'
           frame
    else return $ Just $ s {remainingAttempts = remaining'}

drawCircle :: Circle -> Generate (Render ())
drawCircle (Circle (V2 x y) r) = do
  colour <- fgColour
  return $ do
    arc x y r 0 (2 * pi)
    setColour colour
    fill

data Wave =
  Wave (Double -> Generate Double)

wave :: Generate Wave
wave = do
  World {..} <- asks world
  amplitude <- sampleRVar $ uniform 30 210
  phaseShift <- sampleRVar $ uniform 0 (2 * pi)
  period <- sampleRVar $ uniform 1000 3000
  shiftY <- sampleRVar $ normal 30 10 >>= return . abs
  turbulence <- sampleRVar $ uniform 10 20
  noisScale <- sampleRVar $ uniform 70 100
  return $
    Wave $ \x -> do
      let x' = ((x / period) + phaseShift) * 2 * pi
      let y = height - shiftY - amplitude * sin x'
      noise <-
        noiseSample (V3 (x' / noisScale) (y / noisScale) 0.1) >>=
        return . (* turbulence)
      return $ y + noise

drawWave :: Wave -> Generate (Render ())
drawWave (Wave f) = do
  World {..} <- asks world
  colour <- fgColour
  coords <-
    V.sequence $
    V.generate (floor width) $ \i ->
      let x = fromIntegral i
       in f x >>= return . (\y -> V2 x y)
  let (V2 sx sy) = V.head coords
  let (V2 ex ey) = V.last coords
  let bgCoords =
        V.cons (V2 (sx - 1) (height + 1)) $
        V.snoc coords $ V2 (ex + 1) (height + 1)
  return $ do
    moveTo sx sy
    drawPath bgCoords
    setColour bgColour
    fill
    drawPath coords
    setColour colour
    stroke

drawPath :: V.Vector (V2 Double) -> Render ()
drawPath points = do
  let (V2 sx sy) = V.head points
  moveTo sx sy
  V.foldr1 (>>) $ V.map (\(V2 x y) -> lineTo x y) $ V.tail points

estimateCircle :: Int -> Circle -> V.Vector (V2 Double)
estimateCircle res (Circle c r) =
  V.generate res $ \i ->
    let theta = (fromIntegral i / fromIntegral res) * 2 * pi
     in circumPoint c theta r

moveToward :: V2 Double -> V2 Double -> Double -> V2 Double
moveToward c p d =
  let theta = circumPhase c p
      r = distance c p
   in circumPoint c theta (r - d)

drapeOver ::
     Double
  -> V2 Double
  -> Q.QuadTree Circle
  -> V.Vector (V2 Double)
  -> V.Vector (V2 Double)
drapeOver margin center tree points =
  if done
    then points'
    else drapeOver margin center tree points'
  where
    distanceFrom p =
      negate $
      fromJust $
      fmap ((overlap (Circle p margin)) . (Q.leafTag)) $
      Q.nearest heuristic (Q.Leaf p (Circle p margin)) tree
    update p =
      let dist = distanceFrom p
          step = max (dist / 3 * 2) 3
          p' = moveToward center p step
       in if dist < 0 || distance p' center < 5
            then (False, p)
            else (True, p')
    updates = V.map (update) points
    points' = V.map (snd) updates
    done = not $ V.any (fst) updates

drawLine :: V.Vector (V2 Double) -> Generate (Render ())
drawLine points = do
  colour <- fgColour
  width <- sampleRVar $ uniform 0.1 0.5
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
    V.generate echos $ const $ (sampleRVar $ normal 0 80 >>= return . abs)
  return $
    V.map (\throw -> V.map (\p -> moveToward c p (negate throw)) line) throws

outlineCluster :: Int -> Q.QuadTree Circle -> Generate (V.Vector (V2 Double))
outlineCluster res circles = do
  c <- centerPoint
  margin <- sampleRVar $ uniform 3 7
  let drape = estimateCircle res $ Circle c 800
  let raw = drapeOver margin c circles drape
  let smoothed = iterate (smooth) raw
  return $ smoothed !! 100

driftPointOut :: V2 Double -> Double -> V2 Double -> Generate (V2 Double)
driftPointOut center strength p@(V2 x y) = do
  sample <- noiseSample (V3 x y 0.1)
  let r = distance center p
  let r' = r' + sample * strength
  return $ circumPoint center (circumPhase center p) r'

driftOut ::
     V2 Double
  -> Double
  -> V.Vector (V2 Double)
  -> Generate (V.Vector (V2 Double))
driftOut center strength points =
  V.sequence $ V.map (driftPointOut center strength) points

sun :: Generate (Render ())
sun = do
  center <- centerPoint
  let base = Circle center 100
  let basePoly = estimateCircle 3 base
  strength <- sampleRVar $ uniform 10 50
  let layers = 1
  colour <- fgColour
  polies <-
    V.sequence $
    V.generate layers $ \i ->
      driftOut center (fromIntegral i / fromIntegral layers * strength) basePoly
  return $ do
    setColour colour
    V.foldr1 (>>) $ V.map ((>> fill) . drawPath) polies

scene :: Generate (Render ())
scene = do
  World {..} <- asks world
  let frame =
        Rect (V2 (width / 5) (height / 5)) (width / 5 * 3) (height / 5 * 3)
  let circleSearch = mkSearch 200000 frame
  (CircleSearch tree circles _ _) <-
    iterateMaybeM (search) circleSearch >>= return . last
  resolution <- sampleRVar $ uniform 1000 2000
  outline <- outlineCluster resolution tree
  echoCount <- sampleRVar $ uniform 1 100
  outlines <- echoLine echoCount outline
  outlineDraws <- V.sequence $ V.map (drawLine) outlines
  draws <- sequence $ map (drawCircle) circles
  waveCount <- sampleRVar $ uniform 1 4
  waves <- V.sequence $ V.replicate waveCount wave
  waveDraws <- V.sequence $ V.map (drawWave) waves
  sunDraw <- sun
  return $ do
    setAntialias AntialiasBest
    setColour bgColour
    rectangle 0 0 width height
    fill
    foldr1 (>>) draws
    V.foldr1 (>>) outlineDraws
    return ()

fgColour :: Generate (RGB Double)
fgColour = do
  let palette =
        V.map (hexcolour) $
        V.fromList $ ["7F8BA3", "E7F1FB", "ECD5C4", "CFA969"]
  i <- sampleRVar $ uniform 0 (V.length palette - 1)
  return $ palette V.! i

bgColour :: RGB Double
bgColour = hexcolour "2E2F31" --"8ACEDA"

main :: IO ()
main = runInvocation scene
