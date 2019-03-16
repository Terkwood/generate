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
  }

mkSearch :: Int -> Generate CircleSearch
mkSearch n = do
  frame <- fullFrame
  return $ CircleSearch (Q.new frame) [] n

randomCircle :: Generate Circle
randomCircle
  --radius <- sampleRVar $ normal 20 5 >>= return . (+ 1) . abs
 = do
  let radius = 30.1
  center <- randomPoint
  return $ Circle center radius

valid :: Q.QuadTree Circle -> Circle -> Bool
valid tree c@(Circle center _) =
  let leaf = Q.nearest center tree
   in case leaf of
        Just (Q.Leaf _ nearestCircle) -> not $ overlap c nearestCircle > 0
        Nothing -> True

validNaive :: [Circle] -> Circle -> Bool
validNaive found candidate = not $ any ((> 0) . (overlap candidate)) found

search :: CircleSearch -> Generate (Maybe CircleSearch)
search (CircleSearch _ _ 0) = return Nothing
search s@(CircleSearch tree circles remaining) = do
  let remaining' = remaining - 1
  candidate@(Circle center _) <- randomCircle
  if validNaive circles candidate
    then return $
         Just $
         CircleSearch
           (snd $ Q.insert (Q.Leaf center candidate) tree)
           (candidate : circles)
           remaining'
    else return $ Just $ s {remainingAttempts = remaining'}

drawCircle :: Circle -> Render ()
drawCircle (Circle (V2 x y) r) = arc x y r 0 (2 * pi)

scene :: Generate (Render ())
scene = do
  World {..} <- asks world
  frame <- fullFrame
  circleSearch <- mkSearch 10
  circles <-
    iterateMaybeM (search) circleSearch >>= return . (concatMap foundCircles)
  colors <- sequence $ map (const $ fgColour) circles
  return $ do
    setAntialias AntialiasBest
    setColour bgColour
    rectangle 0 0 width height
    fill
    foldr1 (>>) $
      map
        (\(c, col) -> do
           setColour col
           drawCircle c
           fill) $
      zip circles colors

-- ["8ACEDA", "68BAD5", "373C40", "556173", "E7F5F5"]
fgColour :: Generate (RGB Double)
fgColour = do
  let palette =
        V.map (hexcolour) $
        V.fromList $
        ["0E6E79", "E9BB52", "1D5076", "0EC6B7", "FBF9F4", "DD985F", "D2344A"]
  i <- sampleRVar $ uniform 0 (V.length palette - 1)
  return $ palette V.! i

bgColour :: RGB Double
bgColour = hexcolour "8ACEDA"

main :: IO ()
main = runInvocation scene
