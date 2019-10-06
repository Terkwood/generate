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
import Generate.Collision.Bounds
import Generate.Colour.SimplePalette
import Generate.Colour.THColours
import Generate.Elements.Blob
import Generate.Geom.Frame
import Generate.Patterns.Grid
import Generate.Patterns.Maze
import Generate.Patterns.Noise
import Generate.Patterns.RecursiveSplit
import Generate.Patterns.Sampling
import Generate.Patterns.Splatter
import Generate.Patterns.Tangles
import Generate.Patterns.Walker
import Generate.Patterns.Water
import Generate.Transforms.Warp
import Generate.Transforms.Wiggle
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
    }

start :: Generate State
start = do
  palette <- mkPalette
  thColours <- mkTHColours palette
  return $ State {palette = palette, thColours = thColours}

gNoiseWiggler :: Generate Wiggler
gNoiseWiggler = do
  z <- sampleRVar $ uniform 0 100
  strength <- sampleRVar $ uniform 10 100
  smoothness <- sampleRVar $ uniform 300 800
  return $ mkNoiseWiggler z strength smoothness

gBlobCfg :: SimplePalette -> Generate BlobCfg
gBlobCfg palette = do
  let gColour = fmap Col $ fgColour palette
  let gRadius = sampleRVar $ normal 20 15
  let strokeRealizer = do
        setLineWidth 0.5
        closePath
        stroke
  gridDensity <- sampleRVar $ uniform 20 100
  blocks <-
    fmap (map $ scaleFromCenter 0.1) $
    tiles def {rows = gridDensity, cols = gridDensity}
  let blockMatte = foldr1 (>>) $ map (\b -> draw b >> fill) blocks
  let blockRealizer = alphaMatte blockMatte fill
  let gRealizer =
        randElem $ V.fromList [fill, strokeRealizer, blockRealizer, fill]
  warper <- fmap fromWiggler gNoiseWiggler
  return $ BlobCfg gColour gRadius gRealizer warper

gBlobs :: State -> Generate (Stream Blob)
gBlobs (State {..}) = do
  blobCfg <- gBlobCfg palette
  points <- fmap sampleStream fullFrame
  return $ S.mapM (mkBlob blobCfg) points

gGrid :: State -> Generate ([Rect], Stream (Render ()))
gGrid state@(State {..}) = do
  World {..} <- asks world
  frameScale <- fmap abs $ sampleRVar $ normal 0.7 0.1
  frame <- fmap (scaleFromCenter frameScale) fullFrame
  tiles <-
    recursiveSplit
      def {shouldContinue = \(SplitStatus _ depth) -> pure $ depth < 3}
      frame
  blobCfg <- gBlobCfg palette
  frameColour <- fgColour palette
  let filter rect = do
        let V2 x y = center rect
        noise <- noiseSample $ fmap (/ 500) $ V3 x y 0
        return $ True -- abs noise > 0.2
  bounds <- filterM filter $ map (scaleFromCenter 0.8) tiles
  let mask rect = do
        let blobGen = mkBlob blobCfg
        let blobs :: Stream Blob =
              S.take 20 $ S.mapM blobGen $ sampleStream rect
        blobSrc <- S.fold_ (>>) (pure ()) id $ S.map draw $ blobs
        return $ do alphaMatte (draw rect >> fill) blobSrc
  return $ (map (scaleFromCenter 1.3) bounds, S.mapM mask $ S.each bounds)

gHugGrid :: [Rect] -> SimplePalette -> Generate (Stream (Render ()))
gHugGrid bounds palette = do
  c <- centerPoint
  let touchesBounds p =
        any (\r -> distanceToRect r p < 3) bounds || distance p c < 3
  let stepIn p = moveToward c p 1
  let moveIn p = until touchesBounds stepIn p
  ps <- fmap points $ fullFrame
  let ps' = subdivideN 8 ps
  s1 <- warp (unWarper 1) $ fromJust $ mkLine $ map moveIn ps'
  s2 <- warp (unWarper 1) s1
  let hugs = S.repeat s2
  let scales = S.map ((1 +) . (/ 100)) $ S.each [0 ..]
  let realize line = do
        colour <- fgColour palette
        w <- fmap abs $ sampleRVar $ normal 0.4 0.1
        return $ do
          setLineWidth w
          setColour colour
          draw line
          stroke
  return $ S.mapM realize $ S.zipWith (\h s -> scaleFrom s c h) hugs scales

sketch :: State -> Generate (Stream (Render ()))
sketch state@(State {..}) = do
  blobs <- gBlobs state
  (bounds, grid) <- gGrid state
  hug <- gHugGrid bounds palette
  return $ streamGenerates [background palette] >> grid

main :: IO ()
main = do
  runStatefulInvocation start sketch return
