module Generate.Draw
  ( Drawable(..)
  , Element(..)
  , drawAll
  , realizeAll
  ) where

import Graphics.Rendering.Cairo

import Generate.Monad
import Linear

class Drawable d where
  draw :: d -> Render ()

instance Drawable [V2 Double] where
  draw points = do
    let (V2 x y) = head points
    moveTo x y
    foldr (>>) (pure ()) $ map (\(V2 x y) -> lineTo x y) $ tail points

class Element e where
  realize :: e -> Generate (Render ())

drawAll :: Drawable d => [d] -> Render ()
drawAll ds = foldr (>>) (pure ()) $ map draw ds

realizeAll :: Element e => [e] -> Generate (Render ())
realizeAll es = do
  renders <- sequence $ map realize es
  return $ foldr (>>) (pure ()) renders
