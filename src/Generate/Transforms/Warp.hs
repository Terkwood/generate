module Generate.Transforms.Warp (Warp(..), Warper(..), WarpInput(..)) where

import Linear
import qualified Data.Vector as V
import Data.Maybe
import Data.RVar
import Data.Random.Distribution.Normal
import Data.Default
import qualified Debug.Trace as D

import Generate.Monad
import Generate.Geom
import Generate.Geom.Line
import qualified Generate.Algo.Vec as V

-- A warper should warp the midpoint.
data Warper = Warper (WarpInput -> Generate (V2 Double))

instance Default Warper where
    def = Warper $ \WarpInput {..} -> do
        let V2 x y = subject
        let variance = distance leftNeighbor rightNeighbor
        let offset = normal 0 $ sqrt variance
        xd <- sampleRVar $ offset
        yd <- sampleRVar $ offset
        return $ V2 (x + xd) $ y + yd

data WarpInput = WarpInput { leftNeighbor :: V2 Double, subject :: V2 Double, rightNeighbor :: V2 Double }

class Warp w where
    warp :: Warper -> w -> Generate w

instance Warp Line where
    warp (Warper f) line = do
        let vs = toVertices line
        let last = V.last vs
        let first = V.head vs
        let vs' = V.snoc (V.cons last vs) first
        let windows = D.trace (show $ V.length vs) V.windows 3 vs'
        vs'' <- mapM (\vs -> f $ WarpInput (vs V.! 0) (vs V.! 1) (vs V.! 2)) windows
        return $ fromJust $ mkLine $ V.fromList vs''