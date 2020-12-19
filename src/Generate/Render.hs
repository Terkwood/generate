module Generate.Render
  ( RenderSpec(..)
  , mkRender
  , mkNoise
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
import Linear

import Generate.Monad
import Generate.Stream

data RenderSpec a = RenderSpec
  { renderCtx :: Int -> Context
  , renderRealizer :: IO (Stream (Render ()))
  , renderEndFrame :: Int
  , renderBrainstorm :: Bool
  , renderDimensions :: V2 Int
  , renderRNG :: PureMT
  }

mkRender ::
     World -> IO (Stream (Render ())) -> Int -> Bool -> Int -> IO (RenderSpec a)
mkRender world realizer endFrame brainstorm seed = do
  renderCtx <- newIORef ()
  return
    RenderSpec
      { renderCtx =
          \frame ->
            Context
              world
              frame
              endFrame
              (fromIntegral frame / 24)
              (mkNoise seed)
              seed
      , renderRealizer = realizer
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

renderFrame :: RenderSpec a -> Int -> IO Surface
renderFrame spec@(RenderSpec {renderDimensions = V2 w h, ..}) frame = do
  layers :: Stream (Render ()) <- renderRealizer
  surface <- createImageSurface FormatARGB32 w h
  let ctx = renderCtx frame
  let scale = scaleFactor $ world ctx
  prepareSurface scale surface
  S.mapM_ pure $ runStream scale ctx renderRNG layers surface
  return surface

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
