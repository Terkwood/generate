module Petal
  ( Petal(..)
  , mkPetal
  ) where

import Linear

import Generate
import Generate.Colour.THColours
import Generate.Patterns.Water
import Generate.Patterns.Wiggle

data Petal =
  Petal
    { _petalRoot :: V2 Double
    , _petalSize :: Double
    , _petalCurve :: [BezierControlPoints]
    , _petalColour :: (RGB Double, Double)
    }

instance Drawable Petal where
  draw (Petal _ _ curve colour) = do
    setColour colour
    draw curve

instance Translucent Petal where
  setOpacity opacity petal@Petal {..} =
    let (col, _) = _petalColour
     in petal {_petalColour = (col, opacity)}

instance Wiggle Petal where
  wiggle wiggler petal@Petal {..} = do
    curve' <- sequence $ map (wiggle wiggler) _petalCurve
    return $ petal {_petalCurve = curve'}

mkPetal :: THColours -> Double -> V2 Double -> Double -> Generate Petal
mkPetal palette size root@(V2 rx ry) theta = do
  let orient = rotateAbout root $ theta - pi / 2
  let left = orient $ (V2 (rx - size / 2) (ry - size))
  let right = orient $ V2 (rx + size / 2) (ry - size)
  let curve = mkCompositeCurve $ mkBezierCurve2d root left right root
  let curve' = subdivideN 4 curve
  wigglePower <- sampleRVar (normal 0 (size / 20)) >>= return . abs
  let wiggler = mkRadialWiggler wigglePower
  curve'' <- sequence $ map (wiggle wiggler) $ realizeCurve curve'
  colour <- assignTHColour palette root
  return $ Petal root size curve'' (colour, 1)
