module Generate.Geom
  ( Poly(..)
  , Subdivisible(..)
  , Center(..)
  , Scale(..)
  , Split(..)
  , ScaleCentered(..)
  ) where

import qualified Data.Vector as V
import Linear

import Generate.Coord
import Generate.Monad

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

class ScaleCentered sc where
  scaleFromCenter :: Double -> sc -> sc

instance (Center sc, Scale sc) => ScaleCentered sc where
  scaleFromCenter factor sc = scaleFrom factor (center sc) sc

class Split s where
  splitOnAxis :: Axis -> Double -> s -> (s, s)
