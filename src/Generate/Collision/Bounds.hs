module Generate.Collision.Bounds
  ( BoundingRect(..)
  , BoundingCircle(..)
  ) where

import Generate.Geom.Circle
import Generate.Geom.Line
import Generate.Geom.Rect

class BoundingRect br where
  boundingRect :: br -> Rect

instance BoundingRect Rect where
  boundingRect = id

class BoundingCircle bc where
  boundingCircle :: bc -> Circle

instance BoundingCircle Circle where
  boundingCircle = id
