module Generate.Render
  ( RenderSpec(..)
  , RenderJob(..)
  , mkRender
  , getRenderedFrame
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

getRenderedFrame :: RenderJob a -> Int -> IO Surface
getRenderedFrame job frame = do
  cached <- H.lookup (cachedFrames job) frame
  cachedPreviousState <- H.lookup (cachedStates job) $ frame - 1
  case cached of
    Just cached -> return cached
    Nothing -> do
      (surface, state) <- render (spec job) cachedPreviousState frame
      H.insert (cachedFrames job) frame surface
      H.insert (cachedStates job) frame state
      return surface

render :: RenderSpec a -> Maybe a -> Int -> IO (Surface, a)
render RenderSpec {..} cachedPreviousState frame = do
  let ctx = renderCtx frame
  let (w, h) = renderDimensions
  let generate = runGenerate ctx renderRNG
  surface <- createImageSurface FormatARGB32 w h
  let state =
        case cachedPreviousState of
          Just cachedState -> generate $ renderStepper cachedState
          Nothing -> generate renderInitState
  let layers :: Generate (Render ()) = renderRealizer state
  renderWith surface $ do
    scale (scaleFactor (world ctx)) (scaleFactor (world ctx))
    setAntialias AntialiasBest
    setSourceRGBA 0 0 0 1
    rectangle 0 0 500 500
    fill
    runGenerate ctx renderRNG layers
  return (surface, state)
