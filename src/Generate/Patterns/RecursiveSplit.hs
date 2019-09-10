module Generate.Patterns.RecursiveSplit
  ( recursiveSplit
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

data SplitStatus =
  SplitStatus
    { p :: V2 Double
    , depth :: Int
    }

data SplitCfg =
  SplitCfg
    { x :: Generate Double
    , y :: Generate Double
    , axis :: Generate Axis
    }

instance Default SplitCfg where
  def =
    SplitCfg
      { x = sampleRVar $ uniform 0.2 0.8
      , y = sampleRVar $ uniform 0.2 0.8
      , axis =
          (sampleRVar $ uniform 0 1 :: Generate Int) >>= \i ->
            return $
            if i == 0
              then X
              else Y
      }

data RecursiveSplitCfg =
  RecursiveSplitCfg
    { splitCfg :: SplitStatus -> Generate SplitCfg
    , shouldContinue :: SplitStatus -> Generate Bool
    }

instance Default RecursiveSplitCfg where
  def =
    RecursiveSplitCfg
      { splitCfg = const $ return def
      , shouldContinue = \(SplitStatus _ depth) -> pure $ depth < 3
      }

recursiveSplit ::
     (Split rs, Center rs) => RecursiveSplitCfg -> rs -> Generate [rs]
recursiveSplit cfg rs = _recursiveSplit cfg 0 [] rs

_recursiveSplit ::
     (Split rs, Center rs)
  => RecursiveSplitCfg
  -> Int
  -> [rs]
  -> rs
  -> Generate [rs]
_recursiveSplit cfg@(RecursiveSplitCfg splitCfg shouldContinue) depth acc rs =
  let splitStatus = SplitStatus (center rs) depth
   in do continue <- shouldContinue splitStatus >>= return . not
         if continue
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
             left <- _recursiveSplit cfg (depth + 1) [] rs1
             _recursiveSplit cfg (depth + 1) (acc ++ left) rs2
