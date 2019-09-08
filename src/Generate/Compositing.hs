module Generate.Compositing
  ( alphaMatte
  ) where

import Graphics.Rendering.Cairo

alphaMatte :: Render () -> Render () -> Render ()
alphaMatte matte src = do
  pushGroup
  src
  popGroupToSource
  pushGroup
  matte
  withGroupPattern mask
