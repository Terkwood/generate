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

ramp :: Int -> V.Vector Double
ramp n = V.generate n $ \i -> fromIntegral i / fromIntegral (n - 1)

data PetalSpawnPoint = PetalSpawnPoint
  { _petalSpawnPointPoint :: V2 Double
  , _petalSpawnPointTheta :: Double
  , _petalSpawnPointThickness :: Double
  , _petalSpawnPointSize :: Double
  }

spawnBand :: THColours -> PetalSpawnPoint -> Generate [Petal]
spawnBand palette (PetalSpawnPoint point theta thickness spread) = do
  let pointSampler = sampleRVar $ normal 0 spread
  radii <- sequence $ take (min 10 (floor thickness)) $ repeat pointSampler
  variance <- sampleRVar $ normal 0 (min 0.5 thickness)
  let thetaForDist radius =
        sampleRVar $
        uniform theta $
        theta +
        if radius < 0
          then negate variance
          else variance
  let points = map (circumPoint point (theta - pi / 2)) radii
  let points' = V.sortWith (negate . distance point) $ V.fromList points
  sequence $
    map (\(p, r) -> thetaForDist r >>= \r -> mkPetal palette thickness p r) $
    zip (V.toList points') radii

waterPetals :: Int -> [PetalSpawnPoint] -> THColours -> Generate [Petal]
waterPetals layerCount spawnPoints palette = do
  petals <- sequence (map (spawnBand palette) spawnPoints) >>= return . concat
  let wigglePower = maximum $ map _petalSpawnPointSize spawnPoints
  let wiggler = radialWiggler 1
  let opacity = 0.4 / (fromIntegral layerCount)
  waterPetals :: [[Petal]] <-
    sequence (map (flatWaterColour opacity layerCount wiggler) petals)
  return $ concat waterPetals

data NoiseWalker = NoiseWalker
  { _noiseWalkerScale :: Double
  , _noiseWalkerStep :: Double
  }

mkNoiseWalker :: Generate NoiseWalker
mkNoiseWalker = do
  World {..} <- asks world
  scale <- sampleRVar $ uniform (200 / (500 / width)) (2500 / (500 / width))
  step <- sampleRVar $ uniform (5 / (500 / width)) (10 / (500 / width))
  return $ NoiseWalker scale step

stepNoiseWalker ::
     V2 Double -> Int -> NoiseWalker -> Generate [(Double, V2 Double)]
stepNoiseWalker start n walker = do
  (_, path, _) <-
    iterateMaybeM (_stepNoiseWalker start) (n, [], walker) >>= return . last
  return $ path

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
  frame <- fullFrame
  let last@(V2 x y) =
        if null path
          then start
          else snd $ head path
  theta <- noiseWalkerDir w last
  let path' =
        if null path
          then [(theta, start)]
          else path
  let next = circumPoint last theta step
  return $
    if withinRect frame next
      then Just (left - 1, (theta, next) : path', w)
      else Just (0, path', w)

squigglyPath :: Int -> V2 Double -> Generate Line
squigglyPath n start = do
  path <- mkNoiseWalker >>= \w -> stepNoiseWalker start (max n 2) w
  return $ fromJust $ mkLine $ V.fromList $ map snd path

intoSpawnPoints ::
     (Int -> Generate Double)
  -> (Int -> Generate Double)
  -> [(Double, V2 Double)]
  -> Generate [PetalSpawnPoint]
intoSpawnPoints thicknessF spreadF path = do
  let enumerated = zip path [0 ..]
  let mkSpawnPoint ((d, p), i) = do
        spread <- spreadF i
        thickness <- thicknessF i
        return $ PetalSpawnPoint p d thickness spread
  sequence $ map mkSpawnPoint $ zip path [0 ..]

mkPalette :: Generate SimplePalette
mkPalette =
  randElem $
  V.fromList
    [ monoPastelBlue
    , monoPastelRed
    , monoPastelBlue
    , monoPastelPurple
    , jhoto
    , castle
    , metroid
    , gurken
    ]

filterLine :: Line -> Generate (Maybe Line)
filterLine line = do
  scaleFactor <- sampleRVar $ normal 0.8 0.3
  threshold :: Double <- sampleRVar $ uniform 0 1
  visible :: Double <- sampleRVar $ uniform 0 1
  return $
    if visible < threshold
      then Nothing
      else Just $ scaleFrom scaleFactor (center line) line

background :: SimplePalette -> Generate (Render ())
background palette = do
  World {..} <- asks world
  density <- sampleRVar $ uniform 20 100
  lines <- maze gridCfgDefault {rows = density, cols = density}
  lines' <- mapMaybeM filterLine lines
  scale <- sampleRVar $ uniform 10 1000
  let lineShader line = do
        colour <- fgColour palette
        let (V2 x y) = V.head $ toVertices line
        opacity <-
          noiseSample (V3 (x / scale) (y / scale) 0.1) >>=
          return . abs . (* 0.25)
        return $ (colour, opacity)
  lineColours <- sequence $ map lineShader lines'
  let lineDrawer line colour = do
        setColour colour
        draw line
        stroke
  return $ do
    setColour $ bgColour palette
    rectangle 0 0 width height
    fill
    foldr1 (>>) $ map (uncurry $ lineDrawer) $ zip lines' lineColours

scene :: SimplePalette -> Generate (Render ())
scene basePalette = do
  World {..} <- asks world
  frame <- fullFrame
  let spreadF i = return $ (2 / (500 / width)) * (log $ fromIntegral i)
  petalBaseSize <- sampleRVar $ uniform 2 4
  petalVariance <- sampleRVar $ uniform 1 2
  let thicknessF i = do
        petalSize <- sampleRVar $ normal petalBaseSize petalVariance
        return $ petalSize * (log $ fromIntegral i)
  root <- spatialSample (scaleFrom 0.75 (center frame) frame)
  vineCount <- sampleRVar $ uniform 1 50
  spawnPaths <-
    sequence $
    map
      (const $ do
         w <- mkNoiseWalker
         start <- return $ root
         path <- stepNoiseWalker start (floor $ 1000 / (500 / width)) w
         intoSpawnPoints thicknessF spreadF path)
      [0 .. vineCount]
  palettes <- sequence $ map (const $ mkTHColours basePalette) spawnPaths
  layerCount <- sampleRVar $ uniform 1 10
  petalSets <-
    sequence $
    map (\(palette, path) -> waterPetals layerCount path palette) $
    zip palettes spawnPaths
  return $ do
    foldr1 (>>) $ map ((>> fill) . draw) $ concat petalSets
    return ()

main :: IO ()
main = do
  runInvocation $ do
    palette <- mkPalette
    bg <- background palette
    fg <- scene palette
    return $ bg >> fg
