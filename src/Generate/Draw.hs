module Generate.Draw
  ( Drawable(..)
  , Element(..)
  , drawAll
  , realizeAll
  ) where

import Control.Monad.Reader
import Graphics.Rendering.Cairo
import qualified Streaming as S
import qualified Streaming.Prelude as S

import Generate.Monad
import Generate.Stream
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

instance Element (Stream (V2 Double)) where
  realize ps = do
    ps' <- S.toList_ ps
    return $ draw ps'

drawAll :: Drawable d => [d] -> Render ()
drawAll ds = foldr (>>) (pure ()) $ map draw ds

realizeAll :: Element e => [e] -> Stream (Render ())
realizeAll es = S.sequence $ S.each $ map realize es
