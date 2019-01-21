module Generate.Render
  ( RenderSpec(..)
  , RenderJob(..)
  , mkRender
  , getRenderedFrame
  )
where

import           Control.Monad.Reader
import           Control.Monad.State
import qualified Data.HashTable.IO             as H
import           Data.IORef
import           Data.RVar
import           Data.Random
import           Data.Random.Distribution.Normal
import           Data.Random.Distribution.Uniform
import           Data.Random.Source.PureMT
import qualified Data.Vector                   as V
import           Graphics.Rendering.Cairo
import           Math.Noise.Modules.Perlin

import           Generate.Monad

data RenderSpec = RenderSpec
  { renderCtx :: Int -> Context
  , renderLayers :: Generate (Render ())
  , renderEndFrame :: Int
  , renderBrainstorm :: Bool
  , renderDimensions :: (Int, Int)
  , renderRNG :: PureMT
  }

data RenderJob = RenderJob
  { spec :: RenderSpec
  , cachedFrames :: H.BasicHashTable Int Surface
  }

mkRender :: World -> Generate (Render ()) -> Int -> Bool -> Int -> IO RenderJob
mkRender world layers endFrame brainstorm seed = do
  spec'         <- spec
  cachedFrames' <- H.new
  return RenderJob { spec = spec', cachedFrames = cachedFrames' }
 where
  spec = do
    renderCtx <- newIORef ()
    return RenderSpec
      { renderLayers     = layers
      , renderEndFrame   = endFrame
      , renderCtx        = \frame -> Context world frame (mkNoise seed) seed
      , renderBrainstorm = brainstorm
      , renderDimensions = scaledDimensions world
      , renderRNG        = pureMT $ fromInteger $ toInteger seed
      }

mkNoise :: Int -> Perlin
mkNoise seed = Perlin
  { perlinSeed        = seed
  , perlinLacunarity  = 2.4
  , perlinOctaves     = 3
  , perlinPersistence = 0.5
  , perlinFrequency   = 1
  }

getRenderedFrame :: RenderJob -> Int -> IO Surface
getRenderedFrame job frame = do
  cached <- H.lookup (cachedFrames job) frame
  case cached of
    Just cached -> return cached
    Nothing     -> do
      surface <- render (spec job) frame
      H.insert (cachedFrames job) frame surface
      return surface

render :: RenderSpec -> Int -> IO Surface
render RenderSpec {..} frame = do
  let ctx    = renderCtx frame
  let (w, h) = renderDimensions
  surface <- createImageSurface FormatARGB32 w h
  renderWith surface $ do
    scale (scaleFactor (world ctx)) (scaleFactor (world ctx))
    setAntialias AntialiasBest
    setSourceRGBA 0 0 0 1
    rectangle 0 0 500 500
    fill
    runGenerate ctx renderRNG renderLayers
  return surface
