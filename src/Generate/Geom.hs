module Generate.Geom
  ( Poly(..)
  , Subdivisible(..)
  , Center(..)
  , Scale(..)
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

class Center c where
  center :: c -> V2 Double

class Scale s where
  scaleFrom :: Double -> V2 Double -> s -> s
