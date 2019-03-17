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
  radius <- sampleRVar $ normal 3 2 >>= return . (+ 1) . abs
  center <- randomPointIn frame
  return $ Circle center radius

valid :: Q.QuadTree Circle -> Circle -> Bool
valid tree c@(Circle center _) =
  let heuristic :: Q.Heuristic Circle
      heuristic =
        Q.Heuristic
          { heuristicDistance =
              \(Q.Leaf _ c1) (Q.Leaf _ c2) -> negate $ overlap c1 c2
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
      leaf = Q.nearest heuristic (Q.Leaf center c) tree
   in case leaf of
        Just (Q.Leaf _ nearestCircle) -> not $ overlap c nearestCircle > 0
        Nothing -> True

validNaive :: [Circle] -> Circle -> Bool
validNaive found candidate = not $ any ((> 0) . (overlap candidate)) found

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

scene :: Generate (Render ())
scene = do
  World {..} <- asks world
  let frame =
        Rect (V2 (width / 5) (height / 5)) (width / 5 * 3) (height / 5 * 3)
  let circleSearch = mkSearch 20000 frame
  circles <-
    iterateMaybeM (search) circleSearch >>= return . foundCircles . last
  draws <- sequence $ map (drawCircle) circles
  return $ do
    setColour bgColour
    rectangle 0 0 width height
    fill
    foldr1 (>>) draws
    return ()

fgColour :: Generate (RGB Double)
fgColour = do
  let palette =
        V.map (hexcolour) $
        V.fromList $ ["68BAD5", "373C40", "556173", "E7F5F5", "31425e"]
  i <- sampleRVar $ uniform 0 (V.length palette - 1)
  return $ palette V.! i

bgColour :: RGB Double
bgColour = hexcolour "8ACEDA"

main :: IO ()
main = runInvocation scene
