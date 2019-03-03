module Draw
  ( Drawable(..)
  ) where

import Graphics.Rendering.Cairo

class Drawable d where
  draw :: d -> Render ()
