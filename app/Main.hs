module Main where

import qualified Data.Vector as V
import Linear hiding (rotate)
import System.IO.Unsafe

import Control.Exception
import Debug.Trace as D
import Generate
import qualified Generate.Algo.CirclePack as P
import qualified Generate.Algo.QuadTree as Q
import qualified Generate.Algo.Vec as V
import Generate.Colour.SimplePalette
import Generate.Colour.THColours
import Generate.Geom.Frame
import Generate.Patterns.Grid
import Generate.Patterns.Maze
import Generate.Patterns.RecursiveSplit
import Generate.Patterns.Sampling
import Generate.Patterns.Water
import Generate.Patterns.Wiggle
import qualified Streaming as S
import qualified Streaming.Prelude as S
import System.IO.Unsafe

ramp :: Int -> [Double]
ramp total = map valueOf [0 .. total]
  where
    valueOf i = (fromIntegral i) / (fromIntegral total)

mkPalette :: Generate SimplePalette
mkPalette =
  randElem $
  V.fromList
    [ mkSimplePalette
        "030303"
        ["68A793", "ECBF1F", "E2B01A", "B95928", "8F253F"]
    , mkSimplePalette "211721" ["4A294D", "F3237F", "DC5956", "F383D0"]
    , mkSimplePalette "EFC271" ["3E8A79", "E9A931", "F03E4D", "CC3433"]
    , mkSimplePalette "A35A49" ["35322B", "FDC8D0", "E69A9A", "FCFEFD"]
    ]

background :: SimplePalette -> Generate (Render ())
background palette = do
  World {..} <- asks world
  return $ do
    setColour $ bgColour palette
    rectangle 0 0 width height
    fill

bgStream :: Stream (Render ())
bgStream = do
  palette <- lift $ mkPalette
  render <- lift $ background palette
  S.yield render

data State =
  State
    { palette :: SimplePalette
    , thColours :: THColours
    , pathOrigin :: V2 Double
    , wiggler :: Wiggler
    }

start :: Generate State
start = do
  palette <- mkPalette
  thColours <- mkTHColours palette
  pathOrigin <- centerPoint
  strength <- sampleRVar $ uniform 0 20
  scale <- sampleRVar $ uniform 10 100
  let wiggler = mkNoiseWiggler 100 strength scale
  return $
    State
      { palette = palette
      , thColours = thColours
      , pathOrigin = pathOrigin
      , wiggler = wiggler
      }

data Mountain =
  Mountain
    { palette :: SimplePalette
    , path :: Stream (V2 Double)
    }

instance Subdivisible Mountain where
  subdivide m@(Mountain {..}) = m {path = subdivide path}

instance Elements Mountain where
  realize (Mountain {..}) = do
    c <- lift $ fgColour palette
    path' <- lift $ S.head_ (realize path) >>= return . fromJust
    S.yield $ do
      setColour c
      path'
      fill

instance Wiggle Mountain where
  wiggle wiggler m@(Mountain {..}) =
    pure $ m {path = S.mapM (wiggle wiggler) path}

mkMountain :: State -> Generate Mountain
mkMountain State {..} = do
  World {..} <- asks world
  mountainWidth <- sampleRVar $ uniform (width / 10) (width / 2)
  mountainHeight <- sampleRVar $ uniform (height / 10) (height / 5)
  offset <- sampleRVar $ uniform (width / 10) (width - width / 10)
  V2 _ y <- centerPoint
  let middleX = offset + (mountainWidth / 2)
  return $
    Mountain palette $
    S.each
      [ V2 offset y
      , V2 middleX (y - mountainHeight)
      , V2 (offset + mountainWidth) y
      ]

sketch :: State -> Generate (Stream (Render ()))
sketch state@(State {..}) = do
  mountain <- mkMountain state
  mountain' <- wiggle wiggler $ subdivideN 8 mountain
  return $ streamGenerates [background palette] >> realize mountain'

main :: IO ()
main = do
  runStatefulInvocation start sketch return
