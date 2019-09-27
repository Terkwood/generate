module Generate.Patterns.Splatter
  ( SplatterCfg(..)
  , Splatter
  , symSplatter
  , mkSplatter
  ) where

import Data.RVar
import Linear

import Generate.Monad
import Generate.Patterns.Sampling

data SplatterCfg =
  SplatterCfg
    { yDelta :: RVar Double
    , xDelta :: RVar Double
    }

data Splatter =
  Splatter
    { cfg :: SplatterCfg
    , origin :: V2 Double
    }

symSplatter :: RVar Double -> SplatterCfg
symSplatter v = SplatterCfg v v

mkSplatter :: SplatterCfg -> V2 Double -> Splatter
mkSplatter cfg origin = Splatter cfg origin

instance Sample Splatter where
  spatialSample (Splatter (SplatterCfg {..}) (V2 x y)) = do
    xd <- sampleRVar $ xDelta
    yd <- sampleRVar $ yDelta
    return $ V2 (x + xd) $ y + yd
