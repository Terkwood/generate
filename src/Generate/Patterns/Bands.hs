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

bandsOnAxis :: Split s => Axis -> [Double] -> s -> [s]
bandsOnAxis axis ts s =
  let ts' =
        head ts :
        (map (\(prev, ts) -> (ts - prev) * (1 - prev)) $ zip ts (tail ts))
      (_, bands) =
        mapAccumR
          (\acc t ->
             let (r, acc') = splitOnAxis axis t acc
              in (acc', r))
          s
          ts'
   in bands
