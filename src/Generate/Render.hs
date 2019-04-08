module Generate.Render
  ( RenderSpec(..)
  , RenderJob(..)
  , mkRender
  , getRenderedFrame
  , getRender
  , render
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

import Generate.Monad

data RenderSpec a = RenderSpec
  { renderCtx :: Int -> Context
  , renderInitState :: Generate a
  , renderRealizer :: a -> Generate (Render ())
  , renderStepper :: a -> Generate a
  , renderEndFrame :: Int
  , renderBrainstorm :: Bool
  , renderDimensions :: (Int, Int)
  , renderRNG :: PureMT
  }

data RenderJob a = RenderJob
  { spec :: RenderSpec a
  , cachedFrames :: H.BasicHashTable Int Surface
  , cachedStates :: H.BasicHashTable Int a
  }

mkRender ::
     World
  -> Generate a
  -> (a -> Generate (Render ()))
  -> (a -> Generate a)
  -> Int
  -> Bool
  -> Int
  -> IO (RenderJob a)
mkRender world initState realizer stepper endFrame brainstorm seed = do
  spec <- spec
  cachedFrames <- H.new
  cachedStates <- H.new
  return $ RenderJob spec cachedFrames cachedStates
  where
    spec = do
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

getRender :: RenderJob a -> Int -> IO (Generate (Render ()))
getRender job frame = do
  let RenderSpec {..} = spec job
  let ctx = renderCtx frame
  cachedPreviousState <- H.lookup (cachedStates job) $ frame - 1
  let state =
        case cachedPreviousState of
          Just cached -> runGenerate ctx renderRNG $ renderStepper cached
          Nothing -> runGenerate ctx renderRNG renderInitState
  H.insert (cachedStates job) frame state
  return $ renderRealizer state

getRenderedFrame :: RenderJob a -> Int -> IO Surface
getRenderedFrame job frame = do
  cached <- H.lookup (cachedFrames job) frame
  layers <- getRender job frame
  case cached of
    Just cached -> return cached
    Nothing -> do
      let (w, h) = renderDimensions $ spec job
      surface <- createImageSurface FormatARGB32 w h
      render (spec job) layers frame surface
      return surface

render :: RenderSpec a -> Generate (Render ()) -> Int -> Surface -> IO ()
render RenderSpec {..} layers frame surface = do
  let ctx = renderCtx frame
  renderWith surface $ do
    scale (scaleFactor (world ctx)) (scaleFactor (world ctx))
    setAntialias AntialiasBest
    setSourceRGBA 0 0 0 1
    rectangle 0 0 500 500
    fill
    runGenerate ctx renderRNG layers
