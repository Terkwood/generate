module Generate.Patterns.RecursiveSplit
  ( RecursiveSplit(..)
  , RecursiveSplitCfg(..)
  , SplitCfg(..)
  , SplitStatus(..)
  ) where

import Data.Default
import Data.RVar
import Data.Random.Distribution.Normal
import Data.Random.Distribution.Uniform
import Linear

import Generate.Coord
import Generate.Geom
import Generate.Monad

data SplitStatus = SplitStatus
  { p :: V2 Double
  , depth :: Int
  }

data SplitCfg = SplitCfg
  { x :: Generate Double
  , y :: Generate Double
  , axis :: Generate Axis
  }

instance Default SplitCfg where
  def =
    SplitCfg
      { x = sampleRVar $ normal 0.5 0.1
      , y = sampleRVar $ normal 0.5 0.1
      , axis =
          (sampleRVar $ uniform 0 1 :: Generate Int) >>= \i ->
            return $
            if i == 0
              then X
              else Y
      }

data RecursiveSplitCfg = RecursiveSplitCfg
  { splitCfg :: SplitStatus -> Generate SplitCfg
  , shouldContinue :: SplitStatus -> Bool
  }

instance Default RecursiveSplitCfg where
  def =
    RecursiveSplitCfg
      { splitCfg = const $ return def
      , shouldContinue = \(SplitStatus _ depth) -> depth < 4
      }

class RecursiveSplit rs where
  recursiveSplit :: RecursiveSplitCfg -> rs -> Generate [rs]

instance (Split rs, Center rs) => RecursiveSplit rs where
  recursiveSplit cfg rs = split cfg 0 [] rs

split ::
     (Split rs, Center rs)
  => RecursiveSplitCfg
  -> Int
  -> [rs]
  -> rs
  -> Generate [rs]
split cfg@(RecursiveSplitCfg splitCfg shouldContinue) depth acc rs =
  let splitStatus = SplitStatus (center rs) depth
   in if not $ shouldContinue splitStatus
        then return $ rs : acc
        else do
          (SplitCfg xM yM axisM) <- splitCfg splitStatus
          axis <- axisM
          x <- xM
          y <- yM
          let t =
                case axis of
                  X -> x
                  Y -> y
          let (rs1, rs2) = splitOnAxis axis t rs
          left <- split cfg (depth + 1) [] rs1
          split cfg (depth + 1) (acc ++ left) rs2
