module Main where

import qualified Data.Vector as V
import Linear hiding (rotate)
import System.IO.Unsafe

import Generate
import qualified Generate.Algo.QuadTree as Q
import qualified Generate.Algo.Vec as V
import Generate.Colour.SimplePalette
import Generate.Colour.THColours
import Generate.Patterns.Grid
import Generate.Patterns.Maze
import Generate.Patterns.Sampling
import Generate.Patterns.Water
import Petal

ramp :: Int -> [Double]
ramp total = map valueOf [0 .. total]
  where
    valueOf i = (fromIntegral i) / (fromIntegral total)

mkPalette :: Generate SimplePalette
mkPalette =
  randElem $
  V.fromList
    [ jhoto
    , metroid
    , mkSimplePalette "EFC271" ["3E8A79", "E9A931", "F03E4D", "CC3433"]
    ]

mkNoiseWiggler :: Double -> Double -> Double -> Wiggler
mkNoiseWiggler z strength smoothness =
  Wiggler $ \p@(V2 x y) -> do
    let scale = 1 / smoothness
    let fixSamplePoint = fmap (scale *)
    theta <-
      (noiseSample $ fixSamplePoint $ V3 x y z) >>= return . (\x -> x * 2 * pi)
    r <-
      (noiseSample $ fixSamplePoint $ V3 x y (negate z)) >>=
      return . (* strength)
    return $ circumPoint p theta r

data Blob = Blob
  { line :: [Generate (V2 Double)]
  , colour :: (RGB Double, Double)
  }

mkBlob :: Double -> Double -> Double -> THColours -> Generate Blob
mkBlob baseSize frameScale smoothness palette = do
  World {..} <- asks world
  frame <- fullFrame >>= return . scaleFromCenter frameScale
  center <- spatialSample frame
  r <- (sampleRVar $ normal 0 (baseSize / 2)) >>= return . (+ baseSize)
  let circle = Circle center r
  z <- sampleRVar $ uniform 0 1000
  deformity <- (sampleRVar $ normal 0 r) >>= return . (+ (r / 4)) . abs
  let wiggler = mkNoiseWiggler z deformity smoothness
  let mkCircumPoint theta = do
        let p = circumPoint center theta r
        wiggle wiggler p
  colour <- assignTHColour palette center
  return $
    Blob
      { line = map mkCircumPoint $ map (\x -> x * 2 * pi) $ ramp $ floor width
      , colour = (colour, 1.0)
      }

instance Wiggle Blob where
  wiggle (Wiggler f) blob@(Blob line _) = do
    line_ <- sequence line
    line' <- sequence $ map f line_
    return $ blob {line = map pure line'}
    -- TODO: Make colored class to get these things for free

instance Translucent Blob where
  setOpacity opacity blob@(Blob _ (c, o)) = blob {colour = (c, opacity)}

mkBlobPool :: Double -> THColours -> Generate [Blob]
mkBlobPool smoothness palette = do
  baseSize <- sampleRVar $ uniform 20 50
  frameScale <- sampleRVar $ uniform 0.4 1.0
  blobCount <- sampleRVar $ uniform 5 15
  sequence $
    take blobCount $ repeat $ mkBlob baseSize frameScale smoothness palette

instance Element Blob where
  realize (Blob line colour) = do
    line_ <- sequence line
    return $ do
      setColour colour
      draw line_
      closePath
      fill

background :: SimplePalette -> Generate (Render ())
background palette = do
  World {..} <- asks world
  return $ do
    setColour $ bgColour palette
    rectangle 0 0 width height
    fill

scene :: THColours -> Generate (Render ())
scene basePalette = do
  World {..} <- asks world
  frame <- fullFrame
  smoothness <- sampleRVar $ uniform 10 1000
  blobs <- mkBlobPool smoothness basePalette
  wigglerVariance <- sampleRVar $ uniform 1 5
  let mkWiggler _ = do
        z <- sampleRVar $ uniform 0 1000
        deformity <- sampleRVar $ normal 0 wigglerVariance >>= return . abs
        return $ mkNoiseWiggler z deformity smoothness
  let mkSplotch blob i = do
        opacity <- sampleRVar $ uniform 0.5 0.7
        layers <- sampleRVar $ uniform 5 10
        flatWaterColour opacity layers mkWiggler blob
  blobSplotches :: [[Blob]] <-
    sequence $ map (uncurry mkSplotch) $ zip blobs [0 ..]
  realizeAll $ concat blobSplotches

main :: IO ()
main = do
  runInvocation $ do
    palette <- mkPalette
    fgPalette <- mkTHColours palette
    bg <- background palette
    fg <- scene fgPalette
    return $ bg >> fg
