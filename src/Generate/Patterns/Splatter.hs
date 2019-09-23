module Generate.Patterns.Splatter (SplatterCfg(..), symSplatter, mkSplatter) where

import Linear
import qualified Streaming.Prelude as S
import Data.RVar

import Generate.Monad
import Generate.Stream

data SplatterCfg = SplatterCfg { yDelta :: RVar Double, xDelta :: RVar Double}

symSplatter :: RVar Double -> SplatterCfg
symSplatter v = SplatterCfg v v

mkSplatter :: SplatterCfg -> V2 Double -> Stream (V2 Double)
mkSplatter (SplatterCfg {..}) (V2 x y) = S.repeatM splat
    where
        splat :: Generate (V2 Double)
        splat = do
            xd <- sampleRVar $ xDelta
            yd <- sampleRVar $ yDelta
            return $ V2 (x + xd) $ y + yd