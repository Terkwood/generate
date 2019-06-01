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
import Generate.Patterns.Sampling
import Generate.Patterns.Water
import Petal

ramp :: Int -> V.Vector Double
ramp n = V.generate n $ \i -> fromIntegral i / fromIntegral (n - 1)

gridPetals :: Int -> THColours -> Generate [Petal]
gridPetals layerCount palette = do
  World {..} <- asks world
  count :: Int <- sampleRVar $ uniform 10000 20000
  screen <- fullFrame
  let screen' = scaleFrom 1.2 (center screen) screen
  spawnPoints <-
    sequence $ map (const $ spatialSample screen') [1 .. (count * 50)]
  noiseScale <- sampleRVar $ uniform (width / 3 * 2) (width * 2)
  noiseSamples <-
    sequence $
    map
      (\(V2 x y) -> noiseSample $ V3 (x / noiseScale) (y / noiseScale) 0.4)
      spawnPoints
  let thetas = map (* (2 * pi)) noiseSamples
  baseSize <- sampleRVar $ uniform 30 70
  sizes <-
    sequence $
    map
      (const $ sampleRVar (normal 0 3 >>= return . (+ baseSize) . abs))
      spawnPoints
  petals <-
    sequence $
    map (\(s, p, t) -> mkPetal palette s p t) $ zip3 sizes spawnPoints thetas
  wigglePower <- sampleRVar (normal 0 $ (baseSize / 10)) >>= return . abs
  let wiggler = radialWiggler wigglePower
  waterPetals :: [Petal] <-
    sequence (map (flatWaterColour 0.3 layerCount wiggler) petals) >>=
    return . concat
  return waterPetals

scene :: Generate (Render ())
scene = do
  World {..} <- asks world
  center <- centerPoint
  simplePalette <-
    randElem $ V.fromList [mote, castle, metroid, gurken, redPalette]
  layerCount <- sampleRVar $ uniform 3 6
  petal <- mkTHColours simplePalette >>= gridPetals layerCount
  return $ do
    setColour $ bgColour simplePalette
    rectangle 0 0 width height
    fill
    foldr1 (>>) $ map ((>> fill) . draw) petal
    return ()

main :: IO ()
main = do
  runInvocation scene
