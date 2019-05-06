module Generate.Patterns.Maze
  ( maze
  ) where

import qualified Data.HashSet as S
import Data.Maybe
import Data.RVar
import Data.Random.Distribution.Uniform
import qualified Data.Vector as V
import Linear

import Generate.Geom.Line
import Generate.Monad
import Generate.Patterns.Grid

maze :: GridCfg -> Generate [Line]
maze cfg = do
  vs <- gridVec cfg
  start <- randomGridPoint vs
  _maze vs start S.empty [] []

_maze ::
     V.Vector (V.Vector (V2 Double))
  -> V2 Int
  -> S.Set (V2 Int)
  -> [V2 Int]
  -> [Line]
  -> Generate [Line]
_maze vs p visited path lines = do
  let visited' = S.insert p visited
  let neighbors = V.filter (`S.notMember` visited) $ gridNeighbors vs p
  if V.null neighbors
    then backtrack vs p visited' path lines
    else explore vs p visited' path lines neighbors

explore ::
     V.Vector (V.Vector (V2 Double))
  -> V2 Int
  -> S.Set (V2 Int)
  -> [V2 Int]
  -> [Line]
  -> V.Vector (V2 Int)
  -> Generate [Line]
explore vs p visited path lines neighbors = do
  let path' = p : path
  i <- sampleRVar $ uniform 0 $ (V.length neighbors - 1)
  let p' = neighbors V.! i
  _maze vs p' visited path' lines

backtrack ::
     V.Vector (V.Vector (V2 Double))
  -> V2 Int
  -> S.Set (V2 Int)
  -> [V2 Int]
  -> [Line]
  -> Generate [Line]
backtrack vs p visited path lines = do
  case path of
    [] -> return lines
    path -> do
      let pos = readPoint vs p
      let newLine =
            fromJust $ mkLine $ V.fromList [pos, readPoint vs $ head path]
      let lines' = newLine : lines
      _maze vs (head path) visited (tail path) lines'

readPoint :: V.Vector (V.Vector a) -> V2 Int -> a
readPoint vs (V2 i j) = (vs V.! i) V.! j

gridPoint :: V.Vector (V.Vector a) -> V2 Int -> Maybe (V2 Int)
gridPoint vs p@(V2 i j) = fmap (const $ p) $ vs V.!? i >>= (V.!? j)

randomRow :: V.Vector a -> Generate Int
randomRow vs = sampleRVar $ uniform 0 $ (V.length vs - 1)

randomGridPoint :: V.Vector (V.Vector a) -> Generate (V2 Int)
randomGridPoint vs = do
  i <- randomRow vs
  j <- randomRow $ vs V.! i
  return $ V2 i j

gridNeighbors :: V.Vector (V.Vector a) -> V2 Int -> V.Vector (V2 Int)
gridNeighbors vs (V2 i j) =
  V.mapMaybe (id) $
  V.fromList
    [ gridPoint vs $ V2 i (j + 1)
    , gridPoint vs $ V2 i (j - 1)
    , gridPoint vs $ V2 (i + 1) j
    , gridPoint vs $ V2 (i - 1) j
    ]
