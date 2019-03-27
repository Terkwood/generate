module Patterns.Sampling
  ( Sample(..)
  ) where

import Linear

import Generate.Monad

class Sample s where
  spatialSample :: s -> Generate (V2 Double)
