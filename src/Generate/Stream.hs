module Generate.Stream
  ( Stream
  , liftGenerate
  , runStream
  , streamGenerates
  ) where

import Control.Monad
import Control.Monad.Reader
import Data.Random.Source.PureMT
import Generate.Monad
import Graphics.Rendering.Cairo
import qualified Streaming as S
import qualified Streaming.Prelude as S

type Stream a = S.Stream (S.Of a) Generate ()

type IOStream a = S.Stream (S.Of a) IO ()

liftGenerate :: Generate a -> Stream a
liftGenerate g = do
  r <- lift $ g
  S.yield r

data RunState =
  RunState
    { ctx :: Context
    , rng :: PureMT
    , stream :: Stream (Render ())
    , surface :: Surface
    , scaleFactor :: Double
    }

runStream ::
     Double -> Context -> PureMT -> Stream (Render ()) -> Surface -> IOStream ()
runStream scaleFactor ctx rng ss surface =
  S.unfoldr _runStream (RunState ctx rng ss surface scaleFactor)

_runStream :: RunState -> IO (Either () ((), RunState))
_runStream state@(RunState {..}) =
  case runGenerate ctx rng $ S.next stream of
    (Left _, _) -> pure $ Left ()
    (Right (renderCmd, rest), rng') -> do
      renderWith surface $ do
        setAntialias AntialiasBest
        scale scaleFactor scaleFactor
        renderCmd
      return $ Right ((), state {rng = rng', stream = rest})

streamGenerates :: [Generate a] -> Stream a
streamGenerates = S.sequence . S.each
