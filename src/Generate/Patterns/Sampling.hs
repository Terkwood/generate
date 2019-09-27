module Generate.Patterns.Sampling
  ( Sample(..)
  ) where

import Linear

import Generate.Monad
import Generate.Stream
import qualified Streaming.Prelude as S

class Sample s where
  spatialSample :: s -> Generate (V2 Double)
  sampleStream :: s -> Stream (V2 Double)
  sampleStream s =
    let sample = spatialSample s
     in S.repeatM sample
