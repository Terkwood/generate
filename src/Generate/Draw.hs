module Generate.Draw
  ( Drawable(..)
  , Elements(..)
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

class Elements e where
  realize :: e -> Stream (Render ())

instance Elements (Stream (V2 Double)) where
  realize ps = do
    ps' <- lift $ S.toList_ ps
    S.yield $ draw ps'

drawAll :: Drawable d => [d] -> Render ()
drawAll ds = foldr (>>) (pure ()) $ map draw ds

realizeAll :: Elements e => [e] -> Stream (Render ())
realizeAll es = foldr (>>) (S.each []) $ map realize es
