module Generate.Draw
  ( Drawable(..)
  , Element(..)
  , drawAll
  , realizeAll
  ) where

import Graphics.Rendering.Cairo

import Generate.Monad

class Drawable d where
  draw :: d -> Render ()

class Element e where
  realize :: e -> Generate (Render ())

drawAll :: Drawable d => [d] -> Render ()
drawAll ds = foldr (>>) (pure ()) $ map draw ds

realizeAll :: Element e => [e] -> Generate (Render ())
realizeAll es = do
  renders <- sequence $ map realize es
  return $ foldr (>>) (pure ()) renders
