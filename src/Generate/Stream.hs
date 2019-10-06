module Generate.Stream
  ( Stream
  , liftGenerate
  , runStream
  , streamGenerates
  , unfoldGenerates
  , concatStreams
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

data NestedStream b =
  NestedStream
    { source :: Stream (Stream b)
    , current :: Stream b
    }

stepNested :: NestedStream b -> Generate (Maybe (b, NestedStream b))
stepNested (NestedStream source current) = do
  next <- S.next current
  case next of
    Left () -> do
      nextStream <- S.next source
      case nextStream of
        Left () -> return Nothing
        Right (nextStream, source') ->
          stepNested $ NestedStream source' nextStream
    Right (b, rest) -> return $ Just (b, NestedStream source rest)

concatStreams :: Stream (Stream b) -> Stream b
concatStreams ss = do
  firstStream <- lift $ S.next ss
  case firstStream of
    Left () -> mempty
    Right (firstStream, source) ->
      S.unfoldr _concatStreams $ NestedStream source firstStream

_concatStreams :: NestedStream b -> Generate (Either () (b, NestedStream b))
_concatStreams nested = do
  nextStep <- stepNested nested
  return $
    case nextStep of
      Nothing -> Left ()
      Just (b, rest) -> Right (b, rest)

unfoldGenerates :: Generate [a] -> Stream a
unfoldGenerates generates = S.unfoldr _unfoldGenerates generates

_unfoldGenerates :: Generate [a] -> Generate (Either () (a, Generate [a]))
_unfoldGenerates generates = do
  gs <- generates
  case gs of
    [] -> pure $ Left ()
    g:gs -> return $ Right (g, return $ gs)

streamGenerates :: [Generate a] -> Stream a
streamGenerates = S.sequence . S.each
