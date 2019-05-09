module Generate.Draw
  ( Drawable(..)
  , drawAll
  ) where

import Graphics.Rendering.Cairo

class Drawable d where
  draw :: d -> Render ()

drawAll :: Drawable d => [d] -> Render ()
drawAll ds = foldr (>>) (pure ()) $ map draw ds
