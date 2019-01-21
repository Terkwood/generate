
module Generate
    ( Generate(..)
    , Random(..)
    , World(..)
    , Context(..)
    , screen
    , file
    , timeSeed
    , runInvocation
    , runRand
    )
where

import           Control.Monad.Reader
import           Control.Monad.State           as State
import           Data.Colour.RGBSpace.HSV
import           Data.Colour.SRGB
import           Data.IORef
import           Data.RVar
import           Data.Random
import           Data.Random.Distribution.Normal
import           Data.Random.Distribution.Uniform
import           Data.Random.RVar
import           Data.Random.Source.PureMT
import           Data.Time.Clock.POSIX
import qualified Data.Vector                   as V
import           Graphics.Rendering.Cairo      as Cairo
import           Graphics.UI.Gtk
import           Graphics.UI.Gtk.Gdk.Events
import           Graphics.UI.Gtk.OpenGL.Config
import           Graphics.UI.Gtk.OpenGL.DrawingArea
import           Options

import           Generate.Cmdline
import           Generate.Monad
import           Generate.Render

runInvocation :: Generate (Render ()) -> IO ()
runInvocation layers = runCommand $ \opts args -> do
    seed <- if (optSeed opts) == 0 then timeSeed else return $ optSeed opts
    let world = World (optWidth opts) (optHeight opts) (optScale opts)
    let renderFactory =
            \newSeed -> mkRender world
                                 layers
                                 (optFrames opts)
                                 (optBrainstorm opts)
                                 newSeed
    putStrLn $ "Initial seed is: " ++ (show seed)
    if optSave opts == ""
        then screen seed renderFactory
        else file (optSave opts) seed renderFactory

timeSeed :: IO (Int)
timeSeed = getPOSIXTime >>= \t -> return $ round . (* 1000) $ t

screen :: Int -> (Int -> IO RenderJob) -> IO ()
screen seed renderFactory = do
    initGUI
    window      <- windowNew
    glCfg       <- glConfigNew [GLModeRGBA, GLModeDouble]
    drawingArea <- glDrawingAreaNew glCfg
    job         <- renderFactory seed
    jobRef      <- newIORef $ job
    frameRef    <- newIORef 0
    containerAdd window drawingArea
    timeoutAdd (renderToScreen drawingArea frameRef jobRef) 16
    window `onKeyPress` ui frameRef jobRef renderFactory
    window `onDestroy` mainQuit
    uncurry (windowSetDefaultSize window) $ (renderDimensions . spec) job
    widgetShowAll window
    mainGUI

file :: String -> Int -> (Int -> IO RenderJob) -> IO ()
file path seed renderFactory = do
    job     <- renderFactory seed
    seedRef <- newIORef seed
    let brainStorm = do
            modifyIORef seedRef (+ 1)
            seed <- readIORef seedRef
            job  <- renderFactory seed
            let spec' = spec job
            seedToFile path job { spec = spec' { renderEndFrame = 1 } }
    if renderBrainstorm $ spec $ job
        then V.foldr1 (>>) $ V.generate (renderEndFrame $ spec $ job) $ const
            brainStorm
        else seedToFile path job

seedToFile :: String -> RenderJob -> IO ()
seedToFile path job = do
    let RenderSpec { renderEndFrame, renderCtx, ..} = spec job
    let seed'  = seed $ renderCtx 0
    let (w, h) = renderDimensions
    let
        writeFrame i = do
            frameSurface <- getRenderedFrame job i
            withImageSurface FormatARGB32 w h $ \surface -> do
                renderWith surface $ do
                    setSourceRGBA 0 0 0 1
                    Cairo.rectangle 0 0 (fromIntegral w) (fromIntegral h)
                    fill
                    setSourceSurface frameSurface 0 0
                    Cairo.rectangle 0 0 (fromIntegral w) (fromIntegral h)
                    fill
                surfaceWriteToPNG
                    surface
                    (path ++ "__" ++ (show seed') ++ "__" ++ (show i) ++ ".png")
    sequence $ map (writeFrame) [0 .. renderEndFrame - 1]
    return ()

renderToScreen :: GLDrawingArea -> IORef Int -> IORef RenderJob -> IO Bool
renderToScreen da frameRef jobRef = do
    dw      <- widgetGetDrawWindow da
    frame   <- readIORef frameRef
    job     <- readIORef jobRef
    surface <- getRenderedFrame job frame
    let (width, height) = (renderDimensions . spec) job
    renderWithDrawable dw $ do
        setSourceSurface surface 0 0
        Cairo.rectangle 0 0 (fromIntegral width) (fromIntegral height)
        fill
    return True

ui :: IORef Int -> IORef RenderJob -> (Int -> IO RenderJob) -> Event -> IO Bool
ui frameRef jobRef renderFactory (Key { eventKeyVal, ..}) = do
    case eventKeyVal of
        65307 -> mainQuit
        114   -> do
            modifyIORef frameRef (const 0)
            newSeed <- timeSeed
            putStrLn $ "New seed is: " ++ (show newSeed)
            newJob <- renderFactory newSeed
            modifyIORef jobRef (const newJob)
        _ -> return ()
    return True
ui _ _ _ _ = return True

