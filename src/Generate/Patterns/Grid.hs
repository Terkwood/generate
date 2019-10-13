module Generate.Patterns.Grid
  ( GridCfg(..)
  , TileFocus(..)
  , grid
  , gridCoords
  , gridPixels
  , gridVec
  , tiles
  ) where

import Control.Monad.Reader
import Data.Default
import Data.Maybe
import qualified Data.Vector as V
import Linear

import Generate.Geom.Rect
import Generate.Monad

data TileFocus
  = Center
  | TopLeft
  | TopCenter
  | TopRight
  | BottomLeft
  | BottomRight

focalPoint :: Double -> Double -> TileFocus -> V2 Double -> V2 Double
focalPoint width height focus (V2 topLeftX topLeftY) =
  case focus of
    Center -> V2 (topLeftX + width / 2) (topLeftY + height / 2)
    TopLeft -> V2 topLeftX topLeftY
    TopCenter -> V2 (topLeftX + width / 2) topLeftY
    TopRight -> V2 (topLeftX + width) topLeftY
    BottomLeft -> V2 topLeftX (topLeftY + height)
    BottomRight -> V2 (topLeftX + width) (topLeftY + height)

data GridCfg =
  GridCfg
    { rows :: Int
    , cols :: Int
    , width :: Maybe Double
    , height :: Maybe Double
    , topLeft :: V2 Double
    , tileFocus :: TileFocus
    }

instance Default GridCfg where
  def =
    GridCfg
      { rows = 10
      , cols = 10
      , width = Nothing
      , height = Nothing
      , topLeft = V2 0 0
      , tileFocus = Center
      }

gridPixels :: Int -> Generate [V2 Double]
gridPixels subpixelOrder = do
  World {width, height, scaleFactor, ..} <- asks world
  grid
    def
      { rows = subpixelOrder * (round $ width * scaleFactor)
      , cols = subpixelOrder * (round $ height * scaleFactor)
      , tileFocus = TopLeft
      }

-- row-order grid of points
grid :: GridCfg -> Generate [V2 Double]
grid cfg@(GridCfg {cols, rows, width, height, topLeft, tileFocus}) = do
  (tileWidth, tileHeight) <- tileDims cfg
  let point (c, r) =
        V2 (fromIntegral c * tileWidth) (fromIntegral r * tileHeight)
  let tiles = map (point) $ gridCoords (cols, rows)
  return $ map ((+ topLeft) . (focalPoint tileWidth tileHeight tileFocus)) tiles

gridVec :: GridCfg -> Generate (V.Vector (V.Vector (V2 Double)))
gridVec cfg@(GridCfg {cols, rows, width, height, topLeft, tileFocus}) = do
  (tileWidth, tileHeight) <- tileDims cfg
  let point (c, r) =
        V2 (fromIntegral c * tileWidth) (fromIntegral r * tileHeight)
  let col = V.generate cols (id)
  let tiles = V.generate rows (\i -> V.map (i, ) col)
  return $
    (V.map . V.map)
      ((+ topLeft) . (focalPoint tileWidth tileHeight tileFocus) . point)
      tiles

gridCoords :: (Int, Int) -> [(Int, Int)]
gridCoords (cols, rows) = [(c, r) | c <- [0 .. cols - 1], r <- [0 .. rows - 1]]

tileDims :: GridCfg -> Generate (Double, Double)
tileDims GridCfg {cols, rows, width, height, ..} = do
  World {width = worldWidth, height = worldHeight, ..} <- asks world
  let (width', height') =
        (fromMaybe worldWidth width, fromMaybe worldHeight height)
  return (width' / fromIntegral cols, height' / fromIntegral rows)

tiles :: GridCfg -> Generate [Rect]
tiles cfg = do
  (tileWidth, tileHeight) <- (fmap . fmap) (+ 0.5) $ tileDims cfg
  points <- grid cfg {tileFocus = TopLeft}
  return $ map (\p -> Rect p tileWidth tileHeight) points
