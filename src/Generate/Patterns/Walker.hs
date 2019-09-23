module Generate.Patterns.Walker
  ( Step(..)
  , NoiseWalkerCfg(..)
  , GaussianWalkerCfg(..)
  , mkGaussianWalker
  , mkNoiseWalker
  , chain
  ) where

import "monad-extras" Control.Monad.Extra
import Control.Monad.Reader
import Data.Default
import Data.Maybe
import Data.RVar
import Data.Random.Distribution.Normal
import Data.Random.Distribution.Uniform
import qualified Data.Vector as V
import Linear
import qualified Streaming.Prelude as S

import Generate.Geom.Circle
import Generate.Geom.Line
import Generate.Geom.Rect
import Generate.Monad
import Generate.Stream

data Step = Step
  { stepIdx :: Int
  , theta :: Double
  , position :: V2 Double
  }

data NoiseWalkerCfg = NoiseWalkerCfg
  { scale :: RVar Double
  , stride :: RVar Double
  , z :: Int -> Generate Double
  }

data GaussianWalkerCfg = GaussianWalkerCfg
  { stride :: RVar Double
  , deviation :: RVar Double
  }

firstStep :: V2 Double -> Step
firstStep origin = Step 0 0 origin

chain :: Stream Step -> (Step -> Generate (Maybe (Stream Step))) -> Stream Step
chain ss stitch = S.unfoldr step ss
    where
      step :: Stream Step -> Generate (Either () (Step, Stream Step))
      step ss = do
        next <- S.next ss
        case next of
          Left _ -> return $ Left ()
          Right (step, rest) -> do
            branch <- stitch step
            case branch of
              Just branch -> return $ Right (step, branch)
              Nothing -> return $ Right (step, rest)

mkGaussianWalker :: GaussianWalkerCfg -> V2 Double -> Stream Step
mkGaussianWalker cfg origin = S.iterateM (guassianWalkerStepper cfg) $ pure $ firstStep origin

guassianWalkerStepper :: GaussianWalkerCfg -> Step -> Generate Step
guassianWalkerStepper (GaussianWalkerCfg {..}) (step@Step {..}) = do
  turn <- sampleRVar deviation
  stepStride <- sampleRVar stride
  return $
    step
      { stepIdx = stepIdx + 1
      , theta = turn + theta
      , position = circumPoint position theta stepStride
      }

mkNoiseWalker :: NoiseWalkerCfg -> V2 Double -> Stream Step
mkNoiseWalker cfg origin = S.iterateM (noiseWalkerStepper cfg) $ pure $ firstStep origin

noiseWalkerStepper :: NoiseWalkerCfg -> Step -> Generate Step
noiseWalkerStepper (NoiseWalkerCfg {..}) step@(Step {..}) = do
  stepScale <- sampleRVar scale
  stepStride <- sampleRVar stride
  stepZ <- z stepIdx
  let V2 x y = position
  sample <- noiseSample $ fmap (/ stepScale) $ V3 x y stepZ
  let theta' = sample * pi
  return $
    step
      { stepIdx = stepIdx + 1
      , theta = theta'
      , position = circumPoint position theta stepStride
      }
