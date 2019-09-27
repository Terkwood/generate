module Generate.Transforms.Warp
  ( Warp(..)
  , Warper(..)
  , WarpInput(..)
  ) where

import Data.Default
import Data.Maybe
import Data.RVar
import Data.Random.Distribution.Normal
import qualified Data.Vector as V
import qualified Debug.Trace as D
import Linear

import qualified Generate.Algo.Vec as V
import Generate.Geom
import Generate.Geom.Line
import Generate.Monad

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
    let last = V.last vs
    let first = V.head vs
    let vs' = V.snoc (V.cons last vs) first
    let windows = V.windows 3 vs'
    vs'' <- mapM (\vs -> f $ WarpInput (vs V.! 0) (vs V.! 1) (vs V.! 2)) windows
    return $ fromJust $ mkLine $ V.fromList vs''
