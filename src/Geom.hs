module Geom
  ( Poly(..)
  , Subdivisible(..)
  ) where

import qualified Data.Vector as V
import Linear

class Poly p where
  toVertices :: p -> V.Vector (V2 Double)
  fromVertices :: V.Vector (V2 Double) -> Maybe p

class Subdivisible s where
  subdivide :: s -> s
  subdivideN :: Int -> s -> s
  subdivideN n start = iterate subdivide start !! n
