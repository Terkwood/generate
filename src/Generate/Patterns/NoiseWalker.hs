module Generate.Patterns.NoiseWalker
  ( NoiseWalker(..)
  , NoiseWalkerCfg(..)
  , mkNoiseWalker
  , SquigglyPathCfg(..)
  , squigglyPath
  ) where

import Control.Monad.Extra
import Control.Monad.Reader
import Data.Default
import Data.Maybe
import Data.RVar
import Data.Random.Distribution.Normal
import Data.Random.Distribution.Uniform
import qualified Data.Vector as V
import Linear

import Generate.Geom.Circle
import Generate.Geom.Line
import Generate.Geom.Rect
import Generate.Monad

data NoiseWalker = NoiseWalker
  { scale :: Double
  , step :: Double
  }

data NoiseWalkerCfg = NoiseWalkerCfg
  { scale :: RVar Double
  , step :: RVar Double
  }

instance Default NoiseWalkerCfg where
  def =
    NoiseWalkerCfg
      {scale = sampleRVar $ uniform 200 2500, step = sampleRVar $ uniform 5 10}

mkNoiseWalker :: NoiseWalkerCfg -> Generate NoiseWalker
mkNoiseWalker (NoiseWalkerCfg scaleM stepM) = do
  World {..} <- asks world
  scale <- sampleRVar scaleM
  step <- sampleRVar stepM
  return $ NoiseWalker scale step

stepNoiseWalker ::
     V2 Double -> Int -> NoiseWalker -> Generate [(Double, V2 Double)]
stepNoiseWalker start n walker = do
  (_, path, _) <-
    iterateMaybeM (_stepNoiseWalker start) (n, [], walker) >>= return . last
  return $ path

noiseWalkerDir :: NoiseWalker -> V2 Double -> Generate Double
noiseWalkerDir (NoiseWalker scale _) (V2 x y) = do
  time <- asks frame >>= return . fromIntegral
  sample <- noiseSample $ V3 (x / scale) (y / scale) (time / scale)
  return $ (2 *) . (pi *) $ abs sample

_stepNoiseWalker ::
     V2 Double
  -> (Int, [(Double, V2 Double)], NoiseWalker)
  -> Generate (Maybe (Int, [(Double, V2 Double)], NoiseWalker))
_stepNoiseWalker start (0, _, _) = pure Nothing
_stepNoiseWalker start (left, path, w@(NoiseWalker _ step)) = do
  frame <- fullFrame
  let last@(V2 x y) =
        if null path
          then start
          else snd $ head path
  theta <- noiseWalkerDir w last
  let path' =
        if null path
          then [(theta, start)]
          else path
  let next = circumPoint last theta step
  return $
    if withinRect frame next
      then Just (left - 1, (theta, next) : path', w)
      else Just (0, path', w)

data SquigglyPathCfg = SquigglyPathCfg
  { walkerCfg :: NoiseWalkerCfg
  , length :: Int
  , start :: V2 Double
  }

instance Default SquigglyPathCfg where
  def = SquigglyPathCfg {walkerCfg = def, length = 30, start = V2 0 0}

squigglyPath :: SquigglyPathCfg -> Generate Line
squigglyPath (SquigglyPathCfg cfg n start) = do
  path <- mkNoiseWalker cfg >>= \w -> stepNoiseWalker start (max n 2) w
  return $ fromJust $ mkLine $ V.fromList $ map snd path
