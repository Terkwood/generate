module Generate.Runner
  ( Generate(..)
  , Random(..)
  , World(..)
  , Context(..)
  , screen
  , file
  , timeSeed
  , runInvocation
  , runStatefulInvocation
  , runRand
  , noiseSample
  ) where

import Control.Monad.Reader
import Control.Monad.State as State
import Data.Colour.RGBSpace.HSV
import Data.Colour.SRGB
import Data.IORef
import Data.RVar
import Data.Random
import Data.Random.Distribution.Normal
import Data.Random.Distribution.Uniform
import Data.Random.RVar
import Data.Random.Source.PureMT
import Data.Time.Clock.POSIX
import qualified Data.Vector as V
import Graphics.Rendering.Cairo as Cairo
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Gdk.Events
import Graphics.UI.Gtk.OpenGL.Config
import Graphics.UI.Gtk.OpenGL.DrawingArea
import Options

import Generate.Cmdline
import Generate.Monad
import Generate.Render
import Generate.Stream

runInvocation :: Stream (Render ()) -> IO ()
runInvocation layers = runStatefulInvocation (pure ()) (const $ layers) (return)

runStatefulInvocation ::
     Generate a -> (a -> Stream (Render ())) -> (a -> Generate a) -> IO ()
runStatefulInvocation initState realizer stepper =
  runCommand $ \opts args -> do
    seed <-
      if (optSeed opts) == 0
        then timeSeed
        else return $ optSeed opts
    let world = World (optWidth opts) (optHeight opts) (optScale opts)
    let renderFactory =
          \newSeed ->
            mkRender
              world
              initState
              realizer
              stepper
              (optFrames opts)
              (optBrainstorm opts)
              newSeed
    putStrLn $ "Initial seed is: " ++ (show seed)
    if optSave opts == ""
      then screen seed renderFactory
      else file (optSave opts) seed renderFactory

timeSeed :: IO (Int)
timeSeed = getPOSIXTime >>= \t -> return $ round . (* 1000) $ t

screen :: Int -> (Int -> IO (RenderSpec a)) -> IO ()
screen seed renderFactory = do
  initGUI
  window <- windowNew
  glCfg <- glConfigNew [GLModeRGBA, GLModeDouble]
  drawingArea <- glDrawingAreaNew glCfg
  spec@(RenderSpec {..}) <- renderFactory seed
  specRef <- newIORef $ spec
  frameRef <- newIORef 0
  containerAdd window drawingArea
  timeoutAdd (renderToScreen drawingArea frameRef specRef) 16
  window `onKeyPress` ui frameRef specRef renderFactory
  window `onDestroy` mainQuit
  uncurry (windowSetDefaultSize window) renderDimensions
  widgetShowAll window
  mainGUI

file :: String -> Int -> (Int -> IO (RenderSpec a)) -> IO ()
file path seed renderFactory = do
  spec@(RenderSpec {..}) <- renderFactory seed
  seedRef <- newIORef seed
  let brainStorm = do
        modifyIORef seedRef (+ 1)
        seed <- readIORef seedRef
        seedToFile path $ spec {renderEndFrame = 1}
  if renderBrainstorm
    then V.foldr1 (>>) $ V.generate renderEndFrame $ const brainStorm
    else seedToFile path spec

seedToFile :: String -> RenderSpec a -> IO ()
seedToFile path spec@(RenderSpec {..}) = do
  let seed' = seed $ renderCtx 0
  let (w, h) = renderDimensions
  let writeFrame i = do
        let filePath =
              path ++ "__" ++ (show seed') ++ "__" ++ (show i) ++ ".png"
        surface <- renderFrame spec i
        surfaceWriteToPNG surface filePath
  sequence $ map (writeFrame) [0 .. renderEndFrame - 1]
  return ()

renderToScreen :: GLDrawingArea -> IORef Int -> IORef (RenderSpec a) -> IO Bool
renderToScreen da frameRef specRef = do
  dw <- widgetGetDrawWindow da
  frame <- readIORef frameRef
  spec@(RenderSpec {renderDimensions = (w, h), ..}) <- readIORef specRef
  surface <- renderFrame spec frame
  modifyIORef frameRef (\frame -> (frame + 1) `mod` renderEndFrame)
  renderWithDrawable dw $ do
    setSourceSurface surface 0 0
    Cairo.rectangle 0 0 (fromIntegral w) (fromIntegral h)
    fill
  return True

ui ::
     IORef Int
  -> IORef (RenderSpec a)
  -> (Int -> IO (RenderSpec a))
  -> Event
  -> IO Bool
ui frameRef specRef renderFactory (Key {eventKeyVal, ..}) = do
  case eventKeyVal of
    65307 -> mainQuit
    65507 -> mainQuit
    114 -> do
      modifyIORef frameRef (const 0)
      newSeed <- timeSeed
      putStrLn $ "New seed is: " ++ (show newSeed)
      newSpec <- renderFactory newSeed
      modifyIORef specRef (const newSpec)
    _ -> return ()
  return True
ui _ _ _ _ = return True
