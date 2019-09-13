module Generate.Patterns.NoiseWalker
  ( NoiseWalker(..)
  , NoiseWalkerCfg(..)
  , NoiseStep(..)
  , mkNoiseWalker
  , walkNoiseWalker
  ) where

import "monad-extras" Control.Monad.Extra
import Control.Monad.Reader
import Data.Default
import Data.Maybe
import Data.RVar
import Data.Random.Distribution.Normal
import Data.Random.Distribution.Uniform
import qualified Data.Vector as V
import Linear
import qualified Streaming.Prelude as S

import Generate.Geom.Circle
import Generate.Geom.Line
import Generate.Geom.Rect
import Generate.Monad
import Generate.Stream

data NoiseWalker =
  NoiseWalker
    { scale :: Double
    , step :: Double
    }

data NoiseWalkerCfg =
  NoiseWalkerCfg
    { scale :: RVar Double
    , step :: RVar Double
    }

data NoiseStep =
  NoiseStep
    { stepIdx :: Int
    , theta :: Double
    , position :: V2 Double
    }

instance Default NoiseWalkerCfg where
  def =
    NoiseWalkerCfg
      {scale = sampleRVar $ uniform 1 2000, step = sampleRVar $ uniform 5 10}

mkNoiseWalker :: NoiseWalkerCfg -> Generate NoiseWalker
mkNoiseWalker (NoiseWalkerCfg scaleM stepM) = do
  World {..} <- asks world
  scale <- sampleRVar scaleM
  step <- sampleRVar stepM
  return $ NoiseWalker scale step

walkNoiseWalker :: NoiseWalker -> V2 Double -> Stream NoiseStep
walkNoiseWalker w origin =
  let first = do
        theta <- noiseWalkerDir w origin
        return $ NoiseStep 0 theta origin
   in S.iterateM (_walkNoiseWalker w) first

_walkNoiseWalker :: NoiseWalker -> NoiseStep -> Generate NoiseStep
_walkNoiseWalker w@(NoiseWalker _ step) (NoiseStep i theta last@(V2 x y)) = do
  theta' <- noiseWalkerDir w last
  let next = circumPoint last theta step
  return $ NoiseStep (i + 1) theta' next

noiseWalkerDir :: NoiseWalker -> V2 Double -> Generate Double
noiseWalkerDir (NoiseWalker scale _) (V2 x y) = do
  time <- asks frame >>= return . fromIntegral
  sample <- noiseSample $ V3 (x / scale) (y / scale) (time / scale)
  return $ sample * pi * 6
