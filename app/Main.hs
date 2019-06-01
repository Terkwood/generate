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

data PetalSpawnPoint = PetalSpawnPoint
  { _petalSpawnPointPoint :: V2 Double
  , _petalSpawnPointTheta :: Double
  , _petalSpawnPointThickness :: Double
  }

waterPetals :: Int -> [PetalSpawnPoint] -> THColours -> Generate [Petal]
waterPetals layerCount spawnPoints palette = do
  World {..} <- asks world
  let baseSize = maximum $ map (_petalSpawnPointThickness) spawnPoints
  petals <-
    sequence $
    map (\(PetalSpawnPoint p t s) -> mkPetal palette s p t) spawnPoints
  wigglePower <-
    sampleRVar (uniform (baseSize / 10) (baseSize / 6)) >>= return . abs
  let wiggler = radialWiggler wigglePower
  waterPetals :: [Petal] <-
    sequence (map (flatWaterColour 0.15 layerCount wiggler) petals) >>=
    return . concat
  return waterPetals

data NoiseWalker = NoiseWalker
  { _noiseWalkerScale :: Double
  , _noiseWalkerStep :: Double
  }

mkNoiseWalker :: Generate NoiseWalker
mkNoiseWalker = do
  scale <- sampleRVar $ uniform 1000 5000
  step <- sampleRVar $ uniform 0.1 4.0
  return $ NoiseWalker scale step

stepNoiseWalker ::
     V2 Double -> Int -> NoiseWalker -> Generate [(Double, V2 Double)]
stepNoiseWalker start n walker = do
  (_, path, _) <-
    iterateMaybeM (_stepNoiseWalker start) (n, [], walker) >>= return . last
  return path

noiseWalkerDir :: NoiseWalker -> V2 Double -> Generate Double
noiseWalkerDir (NoiseWalker scale _) (V2 x y) = do
  sample <- noiseSample $ V3 (x / scale) (y / scale) 0.4
  return $ (2 *) . (pi *) $ abs sample

_stepNoiseWalker ::
     V2 Double
  -> (Int, [(Double, V2 Double)], NoiseWalker)
  -> Generate (Maybe (Int, [(Double, V2 Double)], NoiseWalker))
_stepNoiseWalker start (0, _, _) = pure Nothing
_stepNoiseWalker start (left, path, w@(NoiseWalker _ step)) = do
  let last@(V2 x y) =
        if null path
          then start
          else snd $ head path
  theta <- noiseWalkerDir w last
  let next = circumPoint last theta step
  return $ Just (left - 1, (theta, next) : path, w)

squigglyPath :: Int -> V2 Double -> Generate Line
squigglyPath n start = do
  path <- mkNoiseWalker >>= \w -> stepNoiseWalker start (max n 2) w
  return $ fromJust $ mkLine $ V.fromList $ map snd path

intoSpawnPoints ::
     (Double -> Generate Double)
  -> (Int -> Generate Double)
  -> [(Double, V2 Double)]
  -> Generate [PetalSpawnPoint]
intoSpawnPoints thicknessF spreadF path = do
  let enumerated = zip path [0 ..]
  let mkSpawnPoint ((d, p), i) = do
        spread <- spreadF i
        thickness <- thicknessF spread
        side :: Int <- sampleRVar $ uniform 1 2
        let theta =
              if side == 1
                then d + (pi / 2)
                else d - (pi / 2)
        theta' <- sampleRVar $ normal 0 (pi / 5) >>= return . (+ theta)
        let point = circumPoint p theta spread
        return $ PetalSpawnPoint point (theta' - pi) thickness
  sequence $ map mkSpawnPoint $ zip path [0 ..]

scene :: Generate (Render ())
scene = do
  World {..} <- asks world
  frame <- fullFrame
  let randSimplePalette =
        randElem $ V.fromList [monoPastelBlue, monoPastelRed, monoPastelBlue]
  basePalette <- randSimplePalette
  let spreadF i = return $ (fromIntegral i / 100) * 30
  let thicknessF s = return $ 20 - s
  spawnPaths <-
    sequence $
    map
      (const $ do
         w <- mkNoiseWalker
         start <- spatialSample frame
         path <- stepNoiseWalker start 400 w
         intoSpawnPoints thicknessF spreadF path)
      [0 .. 10]
  palettes <-
    sequence $ map (const $ randSimplePalette >>= mkTHColours) spawnPaths
  layerCount <- sampleRVar $ uniform 5 15
  petalSets <-
    sequence $
    map (\(palette, path) -> waterPetals layerCount path palette) $
    zip palettes spawnPaths
  return $ do
    setColour $ bgColour basePalette
    rectangle 0 0 width height
    fill
    foldr1 (>>) $ map ((>> fill) . draw) $ concat petalSets
    return ()

main :: IO ()
main = do
  runInvocation scene
