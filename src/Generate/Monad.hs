module Generate.Monad
  ( Generate(..)
  , runGenerate
  , Context(..)
  , Random(..)
  , World(..)
  , noiseSample
  , runRand
  , scaledDimensions
  ) where

import Control.Monad.Reader
import Control.Monad.State as State
import Data.Maybe
import Data.Random.Source.PureMT
import Graphics.Rendering.Cairo
import Linear
import Math.Noise
import Math.Noise.Modules.Perlin

data World = World
  { width :: Double
  , height :: Double
  , scaleFactor :: Double
  } deriving (Eq, Show)

scaledDimensions :: World -> (Int, Int)
scaledDimensions World {width, height, scaleFactor, ..} =
  (round $ width * scaleFactor, round $ height * scaleFactor)

data Context = Context
  { world :: World
  , frame :: Int
  , noise :: Perlin
  , seed :: Int
  }

type Generate a = StateT PureMT (Reader Context) a

noiseSample :: V3 Double -> Generate (Double)
noiseSample (V3 x y z) = do
  noiseSrc <- asks noise
  return $ fromJust $ getValue noiseSrc (x, y, z)

runGenerate :: Context -> PureMT -> Generate a -> a
runGenerate ctx rng scene =
  (flip runReader ctx) . (>>= (return . fst)) . (flip runStateT rng) $ scene

type Random a = State PureMT a

runRand :: Random a -> Generate a
runRand rand = do
  rng <- State.get
  let (val, rng') = runState rand rng
  State.put rng'
  return val
