module Generate.Runner
  ( Generate(..)
  , Random(..)
  , World(..)
  , Context(..)
  , screen
  , file
  , timeSeed
  , runSketch
  , runRand
  , noiseSample
  ) where

import Control.Monad.Reader
import Control.Concurrent
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
import SDL
import SDL.Event
import SDL.Cairo
import Options

import Generate.Cmdline
import Generate.Monad
import Generate.Render
import Generate.Stream

runSketch :: IO (Stream (Render ())) -> IO ()
runSketch realizer =
  runCommand $ \opts args -> do
    writeFile ".started" "_"
    seed <-
      if (optSeed opts) == 0
        then timeSeed
        else return $ optSeed opts
    let world = World (optWidth opts) (optHeight opts) (optScale opts)
    let renderFactory =
          \newSeed ->
            mkRender
              world
              realizer
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
  initializeAll
  spec@(RenderSpec {..}) <- renderFactory seed
  window <- createWindow "generate" defaultWindow { windowInitialSize = fmap fromIntegral renderDimensions }
  renderer <- createRenderer window (-1) defaultRenderer { rendererTargetTexture = True }
  texture <- createCairoTexture' renderer window
  specRef <- newIORef $ spec
  frameRef <- newIORef 0
  let loop = do
	withCairoTexture' texture $ \surface -> renderToScreen surface frameRef specRef
	rendererDrawColor renderer $= V4 255 255 255 255
	clear renderer
	copy renderer texture Nothing Nothing
	present renderer
	threadDelay 16000 -- sleep 16 ms
	events <- pollEvents
	eventReports :: [Bool] <- mapM (ui frameRef specRef renderFactory) events
	let shouldClose = any id $ eventReports
	unless shouldClose $ loop
  loop
  destroyWindow window

file :: String -> Int -> (Int -> IO (RenderSpec a)) -> IO ()
file path seed renderFactory = do
  spec@(RenderSpec {..}) <- renderFactory seed
  seedRef <- newIORef seed
  let brainStorm = do
        newSeed <- timeSeed
        writeIORef seedRef newSeed
        seedToFile path $
          spec
            { renderCtx =
                \frame ->
                  let ctx = renderCtx frame
                   in ctx {seed = newSeed, noise = mkNoise newSeed}
            , renderEndFrame = 1
            }
  if renderBrainstorm
    then V.foldr1 (>>) $ V.generate renderEndFrame $ const brainStorm
    else seedToFile path spec

seedToFile :: String -> RenderSpec a -> IO ()
seedToFile path spec@(RenderSpec {..}) = do
  let seed' = seed $ renderCtx 0
  let V2 w h = renderDimensions
  let writeFrame i = do
        let filePath =
              path ++ "__" ++ (show seed') ++ "__" ++ (show i) ++ ".png"
        surface <- renderFrame spec i
        surfaceWriteToPNG surface filePath
  sequence $ map (writeFrame) [0 .. renderEndFrame - 1]
  putStrLn $ "Wrote" ++ (show seed') ++ "to file"
  return ()

renderToScreen :: Cairo.Surface -> IORef Int -> IORef (RenderSpec a) -> IO Bool
renderToScreen screen frameRef specRef = do
  frame <- readIORef frameRef
  spec@(RenderSpec {renderDimensions = V2 w h, ..}) <- readIORef specRef
  surface <- renderFrame spec frame
  modifyIORef frameRef (\frame -> (frame + 1) `mod` renderEndFrame)
  renderWith screen $ do
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
ui frameRef specRef renderFactory (Event {eventPayload = KeyboardEvent (KeyboardEventData {keyboardEventKeysym = Keysym {keysymKeycode, .. }, ..})}) = do
  case keysymKeycode of
      Keycode 65307 -> return True -- ESC
      Keycode 65507 -> return True -- Also ESC
      Keycode 114 -> do -- r for 'reseed'
	modifyIORef frameRef (const 0)
	newSeed <- timeSeed
	putStrLn $ "New seed is: " ++ (show newSeed)
	newSpec <- renderFactory newSeed
	modifyIORef specRef (const newSpec)
	return True
      _ -> return True
  return True
ui _ _ _ (Event {eventPayload = WindowClosedEvent _, ..}) = pure True
ui _ _ _ _ = pure False

