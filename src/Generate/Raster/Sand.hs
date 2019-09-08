module Generate.Raster.Sand
  ( sandPaint
  , SandCfg(..)
  ) where

import "monad-extras" Control.Monad.Extra
import Data.Default
import Linear

import qualified Generate.Algo.CirclePack as Pack
import Generate.Collision.Bounds
import Generate.Geom.Circle
import Generate.Monad
import Generate.Patterns.Sampling

data SandCfg =
  SandCfg
    { attempts :: Int
    , mkElement :: V2 Double -> Generate Circle
    }

instance Default SandCfg where
  def = SandCfg {attempts = 1000, mkElement = \p -> pure $ Circle p 2}

sandPaint :: (BoundingRect a, Sample a) => SandCfg -> a -> Generate [Circle]
sandPaint cfg@(SandCfg n _) region = do
  let g = generator cfg region
  let bounds = boundingRect region
  let grainSearch = Pack.mkSearch n bounds g
  let extractCircles = return . Pack.circleSearchFoundCircles . last
  iterateMaybeM Pack.search grainSearch >>= extractCircles

generator :: Sample a => SandCfg -> a -> Generate Circle
generator (SandCfg _ mkElem) region = do
  p <- spatialSample region
  mkElem p
