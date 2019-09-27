module Generate.Patterns.Noise
  ( NoisePattern(..)
  , RadialNoisePattern(..)
  ) where

import Data.Maybe
import Data.RVar
import Data.Random.Distribution.Normal
import Data.Random.Distribution.Uniform
import Linear
import qualified Streaming.Prelude as S

import Generate.Geom.Circle
import Generate.Monad
import Generate.Patterns.Sampling

data NoisePattern s =
  NoisePattern
    { shape :: s
    , scale :: Double
    }

instance (Sample s) => Sample (NoisePattern s) where
  spatialSample (NoisePattern shape scale) =
    let sample = spatialSample shape
        test (V2 x y) = do
          v <- noiseSampleWithScale scale $ V3 x y 0.1
          return $ abs v > 0.3
     in S.head_ (S.take 1 $ S.filterM test $ S.repeatM sample) >>=
        return . fromJust

data RadialNoisePattern =
  RadialNoisePattern
    { origin :: V2 Double
    , scale :: Double
    , radius :: Double
    }

instance Sample RadialNoisePattern where
  spatialSample (RadialNoisePattern origin scale radius) = do
    theta <- sampleRVar $ uniform 0 $ 2 * pi
    noise <- noiseSampleWithScale scale $ V3 theta 0.1 0.1
    r <- sampleRVar $ normal 0 noise
    return $ circumPoint origin theta $ radius * r
