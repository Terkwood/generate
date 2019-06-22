module PetalStroke
  ( PetalStroke(..)
  ) where

import Linear

import Generate
import Generate.Colour.SimplePalette
import Generate.Patterns.Water

data PetalStroke = PetalStroke
  { palette :: SimplePalette
  , root :: V2 Double
  , theta :: Double
  , size :: Double
  , resolution :: Int
  , wiggler :: Wiggler
  }

instance Element PetalStroke where
  realize petal = do
    let n = floor $ size petal
    bands <-
      sequence $
      map
        (band petal)
        [fromIntegral i / (fromIntegral $ n * 2) | i <- [0 .. (n * 2)]]
    let drawBand (c, b) = do
          setColour c
          draw b
          fill
    return $ foldr (>>) (pure ()) $ map drawBand bands

band :: PetalStroke -> Double -> Generate (RGB Double, [BezierControlPoints])
band (PetalStroke palette root theta size resolution wiggler) t = do
  let size' = size * t
  V2 rx ry <- wiggle (radialWiggler $ size / 5) root
  theta' <- sampleRVar $ uniform 0 (2 * pi)
  let orient = rotateAbout root $ theta' - pi / 2
  let left = orient $ (V2 (rx - size' / 2) (ry - size'))
  let right = orient $ V2 (rx + size' / 2) (ry - size')
  let curve = mkCompositeCurve $ mkBezierCurve2d root left right root
  let curve' = subdivideN resolution curve
  curve'' <- sequence $ map (wiggle wiggler) $ realizeCurve curve'
  colour <- fgColour palette
  return (colour, curve'')
