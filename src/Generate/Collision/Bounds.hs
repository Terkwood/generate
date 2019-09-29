module Generate.Collision.Bounds
  ( BoundingRect(..)
  , BoundingCircle(..)
  ) where

import Control.Lens
import Data.Maybe
import Data.Ord
import qualified Data.Vector as V
import Linear

import Generate.Geom
import Generate.Geom.Circle
import Generate.Geom.Line
import Generate.Geom.Rect
import Generate.Geom.Shape

class BoundingRect br where
  boundingRect :: br -> Rect

instance BoundingRect Rect where
  boundingRect = id

instance BoundingRect Line where
  boundingRect line =
    let vs = toVertices line
        V2 left _ = V.minimumBy (comparing (^. _x)) vs
        V2 right _ = V.maximumBy (comparing (^. _x)) vs
        V2 _ top = V.minimumBy (comparing (^. _y)) vs
        V2 _ bottom = V.maximumBy (comparing (^. _y)) vs
     in Rect (V2 left top) (right - left) (bottom - top)

instance BoundingRect Shape where
  boundingRect shape =
    boundingRect $ (fromJust $ fromVertices $ toVertices shape :: Line)

class BoundingCircle bc where
  boundingCircle :: bc -> Circle

instance BoundingCircle Circle where
  boundingCircle = id
