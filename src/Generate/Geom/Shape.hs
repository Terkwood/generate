module Generate.Geom.Shape
  ( Shape
  , mkShape
  , ngon
  ) where

import Data.Maybe
import qualified Data.Vector as V
import Linear

import Generate.Draw
import Generate.Geom
import Generate.Geom.Circle
import Generate.Geom.Line

data Shape =
  Shape (V.Vector (V2 Double))
  deriving (Show)

instance Drawable Shape where
  draw (Shape vertices) =
    let line :: Line = fromJust $ mkLine vertices
     in draw line

instance Poly Shape where
  toVertices (Shape line) = toVertices $ fromJust $ mkLine line
  fromVertices vs = mkShape vs

instance Points Shape where
  points (Shape line) = V.toList line

instance Subdivisible Shape where
  subdivide (Shape line) =
    Shape $
    V.fromList $ drop 1 $ init $ subdivide $ V.toList line ++ [V.head line]

mkShape :: Points p => p -> Maybe Shape
mkShape p =
  let ps = V.fromList $ points p
   in if V.length ps >= 3
        then Just $ Shape ps
        else Nothing

ngon :: Double -> Int -> Double -> V2 Double -> Shape
ngon phase n r origin =
  fromJust $ mkShape $ V.generate n $ \i -> circumPoint origin (phaseOf i) r
  where
    phaseOf i = phase + (fromIntegral i / fromIntegral n) * 2 * pi
