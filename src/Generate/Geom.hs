module Generate.Geom
  ( Poly(..)
  , Subdivisible(..)
  , Center(..)
  , Scale(..)
  , Split(..)
  , ScaleCentered(..)
  , Points(..)
  ) where

import qualified Data.Vector as V
import Linear
import qualified Streaming.Prelude as S

import Generate.Coord
import Generate.Monad
import Generate.Stream

class Poly p where
  toVertices :: p -> V.Vector (V2 Double)
  fromVertices :: V.Vector (V2 Double) -> Maybe p

class Points v where
  points :: v -> [V2 Double]

instance Points [V2 Double] where
  points = id

instance Points (V.Vector (V2 Double)) where
  points = V.toList

class Subdivisible s where
  subdivide :: s -> s
  subdivideN :: Int -> s -> s
  subdivideN n start = iterate subdivide start !! n

instance Subdivisible [V2 Double] where
  subdivide vs =
    if null vs
      then []
      else let first = concat $ zipWith (\a b -> [a, midpoint a b]) vs $ tail vs
            in first ++ [last vs]

instance Subdivisible (Stream (V2 Double)) where
  subdivide vs =
    S.take 1 vs >>
    (S.concat $ S.zipWith (\a b -> [midpoint a b, b]) vs $ S.drop 1 vs)

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
