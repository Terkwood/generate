module Generate.Elements.Blob
  ( Blob(..)
  , BlobCfg(..)
  , mkBlob
  ) where

import Control.Monad.Reader
import Graphics.Rendering.Cairo
import Linear

import Generate.Colour
import Generate.Draw
import Generate.Geom.Circle
import Generate.Geom.Shape
import Generate.Monad
import Generate.Transforms.Warp

data Blob =
  Blob
    { blob :: Shape
    , colour :: Col
    }

instance Draw Blob where
  draw (Blob {..}) = do
    setColour colour
    draw blob
    fill

data BlobCfg =
  BlobCfg
    { gColour :: Generate Col
    , gRadius :: Generate Double
    , warper :: Warper
    }

mkBlob :: BlobCfg -> V2 Double -> Generate Blob
mkBlob (BlobCfg {..}) center = do
  World {..} <- asks world
  radius <- gRadius
  let base = ngon 0 (floor width) radius center
  blob <- warp warper base
  colour <- gColour
  return $ Blob blob colour
