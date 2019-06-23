module Generate.Patterns.Bands
  ( bandsOnAxis
  , BandBounds(..)
  , intoBounds
  ) where

import Data.List

import Generate.Coord
import Generate.Geom

data BandBounds =
  UniformBands Int

intoBounds :: BandBounds -> [Double]
intoBounds (UniformBands n) =
  map ((/ fromIntegral n) . fromIntegral) [1 .. (n - 1)]

data BandSplit = BandSplit
  { splitDeltaAbsolute :: Double
  , bandSize :: Double
  }

bandSplits :: [Double] -> [BandSplit]
bandSplits ts =
  let first = head ts
      first' = BandSplit first 1
      (rest :: [BandSplit]) =
        map (\(ts, prev) -> BandSplit (ts - prev) $ 1 - prev) $ zip ts $ tail ts
   in first' : rest

bandsOnAxis :: Split s => Axis -> [Double] -> s -> [s]
bandsOnAxis axis ts s = _bandsOnAxis axis (bandSplits ts) [] s

_bandsOnAxis :: Split s => Axis -> [BandSplit] -> [s] -> s -> [s]
_bandsOnAxis axis [] acc final = final : acc
_bandsOnAxis axis ((BandSplit split size):rest) acc band =
  let split' = split / size
      (left, right) = splitOnAxis axis split' band
      acc' = right : acc
   in _bandsOnAxis axis rest acc' left
