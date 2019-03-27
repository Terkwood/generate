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
import System.IO.Unsafe

import Algo.CirclePack
import qualified Algo.QuadTree as Q
import Colour
import Coord
import Generate
import Geom.Circle
import Geom.Rect
import Patterns.Grid
import Patterns.Sampling

drawLine :: V.Vector (V2 Double) -> Generate (Render ())
drawLine points = do
  colour <- fgColour
  width <- sampleRVar $ uniform 3 7
  fillMode :: Int <- sampleRVar $ uniform 0 1
  let (V2 x y) = V.head points
  let steps :: V.Vector (Render ()) =
        V.map (\(V2 x y) -> lineTo x y) $ V.tail points
  return $ do
    setColour colour
    setLineCap LineCapButt
    setLineWidth width
    moveTo x y
    V.foldr1 (>>) steps
    closePath
    if fillMode == 1
      then fill
      else stroke

drawCircle :: Circle -> Generate (Render ())
drawCircle (Circle (V2 x y) r) = do
  colour <- fgColour
  return $ do
    arc x y (r * 0.6) 0 (2 * pi)
    setColour colour
    fill

randomCircle :: Rect -> Generate Circle
randomCircle frame = do
  radius <- sampleRVar $ normal 15 4 >>= return . (+ 5) . abs
  center <- spatialSample frame
  return $ Circle center radius

drawPath :: V.Vector (V2 Double) -> Render ()
drawPath points = do
  let (V2 sx sy) = V.head points
  moveTo sx sy
  V.foldr1 (>>) $ V.map (\(V2 x y) -> lineTo x y) $ V.tail points

searchPattern :: Generate Circle
searchPattern = do
  patternCenter <- centerPoint
  dist <- sampleRVar $ normal 150 30 >>= return . abs
  phase <- sampleRVar $ uniform 0 (2 * pi)
  radius <- sampleRVar $ uniform 2 100
  let center = circumPoint patternCenter phase dist
  return $ Circle center radius

class Collidable c where
  colliders :: c -> [Circle]

class Drawable d where
  draw :: d -> Generate (Render ())

data Squiggle = Squiggle
  { squigglePath :: Render ()
  , squiggleColliders :: [Circle]
  }

instance Collidable Squiggle where
  colliders (Squiggle _ cs) = cs

instance Drawable Squiggle where
  draw (Squiggle path _) = do
    colour <- fgColour
    width <- sampleRVar $ uniform 2 7
    return $ do
      setColour colour
      setLineWidth width
      setLineCap LineCapRound
      path
      stroke

squiggly :: Generate Squiggle
squiggly = do
  start@(V2 sx sy) <- randomPoint
  size <- sampleRVar $ normal 15 5 >>= return . abs . (+ 3)
  endPhase <- sampleRVar $ uniform 0 (2 * pi)
  phase1 <- sampleRVar $ uniform 0 (2 * pi)
  phase2 <- sampleRVar $ uniform 0 (2 * pi)
  let end@(V2 ex ey) = circumPoint start endPhase size
  let middle = midpoint start end
  let cpRadius = size / 3 * 2
  let (V2 cp1x cp1y) = circumPoint middle phase1 cpRadius
  let (V2 cp2x cp2y) = circumPoint middle phase2 cpRadius
  let path = do
        moveTo sx sy
        curveTo cp1x cp1y cp2x cp2y ex ey
  let collisionCircles =
        [Circle start (size / 4), Circle middle size, Circle end (size / 4)]
  return $ Squiggle path collisionCircles

data Triangle =
  Triangle (V.Vector (V2 Double))

instance Collidable Triangle where
  colliders (Triangle verts) = map (\p -> Circle p size) $ V.toList verts
    where
      p1 = V.head verts
      p2 = V.last verts
      size = distance p1 p2 / 2

instance Drawable Triangle where
  draw (Triangle verts) = drawLine verts

triangle :: Generate Triangle
triangle = do
  center <- randomPoint
  phase <- sampleRVar $ uniform 0 (2 * pi)
  size <- sampleRVar $ normal 20 5 >>= return . (+ 1) . abs
  let interval = 2 * pi / 3
  let point i = circumPoint center (phase + interval * fromIntegral i) size
  return $ Triangle $ V.generate 3 point

instance Collidable Circle where
  colliders c = [c]

instance Drawable Circle where
  draw circle = drawCircle circle

instance Collidable Ngon where
  colliders (Ngon verts c) = [c]

instance Drawable Ngon where
  draw (Ngon verts c) = drawLine verts

data Ngon =
  Ngon (V.Vector (V2 Double))
       Circle

randomNgon :: Generate Ngon
randomNgon = do
  circle@(Circle center r) <- fullFrame >>= randomCircle
  n :: Int <- sampleRVar $ uniform 3 5
  phase <- sampleRVar $ uniform 0 (2 * pi)
  let interval = 2 * pi / fromIntegral n
  let point i = circumPoint center (phase + interval * fromIntegral i) r
  return $ Ngon (V.generate n point) (Circle center (r * 2))

data Memphis =
  forall m. (Collidable m, Drawable m) =>
            Memphis m

instance Collidable Memphis where
  colliders (Memphis m) = colliders m

instance Drawable Memphis where
  draw (Memphis d) = draw d

memphisOptions :: V.Vector (Generate Memphis)
memphisOptions =
  V.fromList
    [ squiggly >>= return . Memphis
    , squiggly >>= return . Memphis
    , squiggly >>= return . Memphis
    , fullFrame >>= randomCircle >>= return . Memphis
    , randomNgon >>= return . Memphis
    ]

randomMemphis :: Generate Memphis
randomMemphis = do
  index <- sampleRVar $ uniform 0 (V.length memphisOptions - 1)
  memphisOptions V.! index

buildSquigglies ::
     Int
  -> [Memphis]
  -> Q.QuadTree Circle
  -> Generate (Maybe (Int, [Memphis], Q.QuadTree Circle))
buildSquigglies 0 _ _ = pure Nothing
buildSquigglies attempts soFar collisionTree = do
  candidate <- randomMemphis
  let toLeaf c@(Circle center _) = Q.Leaf center c
  let collide collider@(Circle center _) =
        fromMaybe False $
        fmap (\c -> overlap c collider >= 0) $
        fmap Q.leafTag $ Q.nearest heuristic (toLeaf collider) collisionTree
  let valid = not $ any (collide) $ colliders candidate
  let (found, tree') =
        if valid
          then ( candidate : soFar
               , Q.insertMany (map (toLeaf) $ colliders candidate) collisionTree)
          else (soFar, collisionTree)
  return $ Just $ (attempts - 1, found, tree')

leafRadius :: Q.Leaf Circle -> Double
leafRadius = (\(Circle _ r) -> r) . Q.leafTag

representativeNodeUpdater ::
     Maybe (Q.Leaf Circle) -> Q.Leaf Circle -> Maybe (Q.Leaf Circle)
representativeNodeUpdater current new =
  case current of
    Just current -> Just $ maximumBy (comparing leafRadius) [new, current]
    Nothing -> Just new

scene :: Generate (Render ())
scene = do
  World {..} <- asks world
  frame <- fullFrame
  (_, memphese, _) <-
    iterateMaybeM
      (uncurry3 buildSquigglies)
      (10000, [], Q.new (representativeNodeUpdater) frame) >>=
    return . last
  memphisDraws <- sequence $ map (draw) memphese
  col <- fgColour
  return $ do
    setAntialias AntialiasBest
    setColour bgColour
    rectangle 0 0 width height
    fill
    foldr1 (>>) memphisDraws
    return ()

fgColour :: Generate (RGB Double)
fgColour = do
  let palette =
        V.map (hexcolour) $
        V.fromList $ ["9D6425", "773D24", "4B2E1C", "FAF2DA"]
  i <- sampleRVar $ uniform 0 (V.length palette - 1)
  return $ palette V.! i

bgColour :: RGB Double
bgColour = hexcolour "C9A66E" --"F4F1EB" --"8ACEDA"

main :: IO ()
main = runInvocation scene
