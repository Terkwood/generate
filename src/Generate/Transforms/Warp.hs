module Generate.Transforms.Warp
  ( Warp(..)
  , Warper(..)
  , WarpInput(..)
  , fromWiggler
  , unWarper
  ) where

import Data.Default
import Data.Maybe
import Data.RVar
import Data.Random.Distribution.Normal
import qualified Data.Vector as V
import qualified Debug.Trace as D
import Linear

import qualified Generate.Algo.Vec as V
import Generate.Coord
import Generate.Geom
import Generate.Geom.Line
import Generate.Geom.Shape
import Generate.Monad
import Generate.Transforms.Wiggle

-- A warper should warp the midpoint.
data Warper =
  Warper (WarpInput -> Generate (V2 Double))

instance Default Warper where
  def =
    Warper $ \WarpInput {..} -> do
      let V2 x y = subject
      let variance = distance leftNeighbor rightNeighbor
      let offset = normal 0 $ sqrt variance
      xd <- sampleRVar $ offset
      yd <- sampleRVar $ offset
      return $ V2 (x + xd) $ y + yd

data WarpInput =
  WarpInput
    { leftNeighbor :: V2 Double
    , subject :: V2 Double
    , rightNeighbor :: V2 Double
    }

class Warp w where
  warp :: Warper -> w -> Generate w

instance Warp Line where
  warp (Warper f) line = do
    let vs = toVertices line
    let caboose = V.last vs
    let first = V.head vs
    let windows = V.windows 3 vs
    vs' <- mapM (\vs -> f $ WarpInput (vs V.! 0) (vs V.! 1) (vs V.! 2)) windows
    return $ fromJust $ mkLine $ V.fromList $ first : vs' ++ [caboose]

instance Warp Shape where
  warp (Warper f) shape = do
    let vs = toVertices shape
    let caboose = V.last vs
    let first = V.head vs
    let vs' = V.snoc (V.cons caboose vs) first
    let windows = V.windows 3 vs'
    vs'' <- mapM (\vs -> f $ WarpInput (vs V.! 0) (vs V.! 1) (vs V.! 2)) windows
    return $ fromJust $ mkShape $ V.fromList $ vs''

fromWiggler :: Wiggler -> Warper
fromWiggler (Wiggler f) = Warper $ \(WarpInput {..}) -> f subject

unWarper :: Double -> Warper
unWarper strength =
  Warper $ \WarpInput {..} -> do
    let target = midpoint leftNeighbor rightNeighbor
    let error = distance target subject
    let correction = strength * error
    return $ moveToward target subject correction
