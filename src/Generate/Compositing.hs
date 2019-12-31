module Generate.Compositing
  ( background
  , alphaMatte
  ) where

import Control.Monad.Reader
import Graphics.Rendering.Cairo

import Generate.Colour
import Generate.Monad

background :: CairoColour c => c -> Generate (Render ())
background c = do
  World {..} <- asks world
  return $ do
    setColour c
    rectangle 0 0 width height
    fill

alphaMatte :: Render () -> Render () -> Render ()
alphaMatte matte src = do
  pushGroup
  src
  popGroupToSource
  pushGroup
  matte
  withGroupPattern mask
