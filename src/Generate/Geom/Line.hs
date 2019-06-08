module Generate.Geom.Line
  ( Line
  , mkLine
  ) where

import Control.Lens
import qualified Data.Vector as V
import Graphics.Rendering.Cairo as Cairo
import Linear

import Generate.Coord
import Generate.Draw
import Generate.Geom
import Generate.Geom.Circle

data Line =
  Line (V.Vector (V2 Double))

instance Poly Line where
  toVertices (Line v) = v
  fromVertices v = mkLine v

mkLine :: V.Vector (V2 Double) -> Maybe Line
mkLine points =
  if V.length points >= 2
    then Just $ Line points
    else Nothing

instance Drawable Line where
  draw (Line points) = do
    let (V2 x y) = V.head points
    moveTo x y
    V.foldr (>>) (pure ()) $ V.map (\(V2 x y) -> lineTo x y) $ V.tail points

instance Center Line where
  center (Line points) = V2 ((right + left) / 2) ((top + bottom) / 2)
    where
      right = V.maximum xs
      left = V.minimum xs
      top = V.maximum ys
      bottom = V.minimum ys
      ys = V.map (^. _y) points
      xs = V.map (^. _x) points

instance Scale Line where
  scaleFrom factor anchor (Line points) = Line $ V.map scalePoint points
    where
      scalePoint point = circumPoint anchor phase (d * factor)
        where
          d = distance point anchor
          phase = circumPhase anchor point

instance Subdivisible Line where
  subdivide (Line verts) = Line $ V.snoc verts' $ V.last verts
    where
      back = V.tail verts
      verts' =
        V.concatMap (\(a, b) -> V.fromList [a, midpoint a b]) $ V.zip verts back
