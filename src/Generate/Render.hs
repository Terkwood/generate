module Generate.Render
  ( RenderSpec(..)
  , mkRender
  , renderFrame
  ) where

import Control.Monad.Reader
import Control.Monad.State
import qualified Data.HashTable.IO as H
import Data.IORef
import Data.RVar
import Data.Random
import Data.Random.Distribution.Normal
import Data.Random.Distribution.Uniform
import Data.Random.Source.PureMT
import qualified Data.Vector as V
import Graphics.Rendering.Cairo
import Math.Noise.Modules.Perlin
import qualified Streaming as S
import qualified Streaming.Prelude as S

import Generate.Monad
import Generate.Stream

data RenderSpec a =
  RenderSpec
    { renderCtx :: Int -> Context
    , renderInitState :: Generate a
    , renderRealizer :: a -> Stream (Render ())
    , renderStepper :: a -> Generate a
    , renderEndFrame :: Int
    , renderBrainstorm :: Bool
    , renderDimensions :: (Int, Int)
    , renderRNG :: PureMT
    , renderLastState :: Generate a
    }

mkRender ::
     World
  -> Generate a
  -> (a -> Stream (Render ()))
  -> (a -> Generate a)
  -> Int
  -> Bool
  -> Int
  -> IO (RenderSpec a)
mkRender world initState realizer stepper endFrame brainstorm seed = do
  renderCtx <- newIORef ()
  return
    RenderSpec
      { renderCtx = \frame -> Context world frame (mkNoise seed) seed
      , renderInitState = initState
      , renderRealizer = realizer
      , renderStepper = stepper
      , renderEndFrame = endFrame
      , renderBrainstorm = brainstorm
      , renderDimensions = scaledDimensions world
      , renderRNG = pureMT $ fromInteger $ toInteger seed
      , renderLastState = initState
      }

mkNoise :: Int -> Perlin
mkNoise seed =
  Perlin
    { perlinSeed = seed
    , perlinLacunarity = 2.4
    , perlinOctaves = 3
    , perlinPersistence = 0.5
    , perlinFrequency = 1
    }

getRender :: RenderSpec a -> Int -> Generate (Stream (Render ()))
getRender (RenderSpec {..}) frame = do
  state <-
    if frame == 0
      then renderInitState
      else renderLastState
  return $ renderRealizer state

renderFrame :: RenderSpec a -> Int -> IO Surface
renderFrame spec@(RenderSpec {renderDimensions = (w, h), ..}) frame = do
  let layers :: Stream (Render ()) = realize $ getRender spec frame
  surface <- createImageSurface FormatARGB32 w h
  let scale = scaleFactor $ world ctx
  prepareSurface scale surface
  S.mapM_ pure $ runStream scale ctx renderRNG layers surface
  return surface
  where
    realize :: Generate a -> a
    realize = fst . (runGenerate ctx renderRNG)
    ctx = renderCtx frame

realizeCommand :: Surface -> Render () -> IO ()
realizeCommand surface cmd = renderWith surface cmd

prepareSurface :: Double -> Surface -> IO ()
prepareSurface scaleFactor surface =
  renderWith surface $ do
    setAntialias AntialiasBest
    scale scaleFactor scaleFactor
    setSourceRGBA 0 0 0 1
    rectangle 0 0 500 500
    fill
