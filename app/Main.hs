module Main where

import qualified Data.Vector as V
import Linear hiding (rotate)
import System.IO.Unsafe

import Generate
import qualified Generate.Algo.QuadTree as Q
import qualified Generate.Algo.Vec as V
import Generate.Patterns.Sampling

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

randomCircle :: Sample s => s -> Generate Circle
randomCircle space = do
  radius <- sampleRVar $ normal 2 2 >>= return . (+ 1) . abs
  center <- spatialSample space
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
          step = max dist 3
          p' = moveToward center p step
       in if dist < 0 || distance p' center < 5
            then (False, p)
            else (True, p')
    updates = V.map (update) points
    points' = V.map (snd) updates
    done = not $ V.any (fst) updates

roughRelativeExtent :: V2 Double -> V.Vector (V2 Double) -> Double
roughRelativeExtent center vs = (negate 1) * (distance center $ V.head vs)

cmpExtent ::
     V2 Double -> V.Vector (V2 Double) -> V.Vector (V2 Double) -> Ordering
cmpExtent center v1 v2 =
  let f = roughRelativeExtent center
   in compare (f v1) (f v2)

drawLine :: V.Vector (V2 Double) -> Generate (Render ())
drawLine points = do
  colour <- fgColour
  width <- sampleRVar $ uniform 0.1 3.0
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
    fill
    --stroke

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
    V.generate echos $ const $ (sampleRVar $ normal 0 300 >>= return . abs)
  return $
    V.map (\throw -> V.map (\p -> moveToward c p (negate throw)) line) throws

outlineCluster :: Int -> Q.QuadTree Circle -> Generate (V.Vector (V2 Double))
outlineCluster res circles = do
  smoothing <- sampleRVar $ uniform 10 150
  c <- centerPoint
  margin <- sampleRVar $ uniform 3 7
  let drape = estimateCircle res $ Circle c 800
  let raw = drapeOver margin c circles drape
  let smoothed = iterate (smooth) raw
  return $ smoothed !! smoothing

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
  circleCount <- sampleRVar $ uniform 100 3000
  let circleSearch = mkSearch circleCount frame
  (CircleSearch tree circles _ _) <-
    iterateMaybeM (search) circleSearch >>= return . last
  let resolution = 3000
  outline <- outlineCluster resolution tree
  let echoCount = 10000
  outlines <- echoLine echoCount outline
  center <- centerPoint
  let outlines' = V.sortBy (cmpExtent center) outlines
  outlineDraws <- V.sequence $ V.map (drawLine) outlines'
  draws <- sequence $ map (drawCircle) circles
  sunDraw <- sun
  rotation <- sampleRVar $ uniform 0 (2 * pi)
  bgColour_ <- bgColour
  return $ do
    setColour bgColour_
    rectangle 0 0 width height
    fill
    translate (width / 2) (height / 2)
    scale 2 2
    rotate rotation
    --foldr1 (>>) draws
    V.foldr1 (>>) outlineDraws
    --sunDraw
    return ()

data Palette =
  Palette String
          [String]

redPalette :: Palette
redPalette = Palette "F8F5EB" ["AD4749", "F8F5EB", "BFAAB9", "AF849B"]

castle = Palette "FFFFFF" ["C74894", "FDDCB7", "F75856", "66AA79", "07444D"]

mote = Palette "EECDB6" ["A83250", "13192C", "8D4C33", "E08752", "A83250"]

metroid = Palette "ECE9E4" ["4DDCEC", "189CE6", "116FAE", "2D1D39"]

gurken = Palette "FCFDF8" ["3FACA5", "7DDCC6", "F79EA2", "FA5778"]

palettes :: V.Vector Palette
palettes = V.fromList $ [castle, redPalette, mote, metroid, gurken]

palette :: Generate Palette
palette = do
  seed_ <- asks seed
  let i = seed_ `mod` (V.length palettes)
  return $ palettes V.! i

fgColour :: Generate (RGB Double)
fgColour = do
  Palette _ fgs <- palette
  let palette = V.map (hexcolour) $ V.fromList fgs
  i <- sampleRVar $ uniform 0 (V.length palette - 1)
  return $ palette V.! i

bgColour :: Generate (RGB Double)
bgColour = do
  Palette bg _ <- palette
  return $ hexcolour bg

main :: IO ()
main = runInvocation scene
