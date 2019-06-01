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

ramp :: Int -> V.Vector Double
ramp n = V.generate n $ \i -> fromIntegral i / fromIntegral (n - 1)

data Wiggler =
  Wiggler (V2 Double -> Generate (V2 Double))

class Wiggle w where
  wiggle :: Wiggler -> w -> Generate w

class Translucent t where
  setOpacity :: Double -> t -> t

data WaterColourLayering = WaterColourLayering
  { _waterColourLayeringLayerCount :: Int
  , _waterColourLayeringLayerOpacity :: Double
  , _waterColourLayeringDepthOfBranch :: Int
  , _waterColourLayeringDepthPerBranch :: Int
  }

warp :: (Wiggle w, Subdivisible w) => Wiggler -> w -> Generate w
warp wiggler source = wiggle wiggler $ subdivide source

warpN :: (Wiggle w, Subdivisible w) => Wiggler -> Int -> w -> Generate w
warpN wiggler n source = do
  results <- iterateMaybeM (_warpN wiggler) (n, source)
  if null results
    then return source
    else return $ snd (last results)

_warpN ::
     (Wiggle w, Subdivisible w)
  => Wiggler
  -> (Int, w)
  -> Generate (Maybe (Int, w))
_warpN wiggler (n, source) =
  if n < 1
    then return Nothing
    else do
      warped <- warp wiggler source
      return $ Just (n - 1, warped)

flatWaterColour ::
     (Translucent wc, Wiggle wc)
  => Double
  -> Int
  -> Wiggler
  -> wc
  -> Generate [wc]
flatWaterColour opacity layerCount wiggler source = do
  layers <- sequence $ map (const $ wiggle wiggler source) [1 .. layerCount]
  return $ map (setOpacity opacity) layers

waterColour ::
     (Translucent wc, Wiggle wc, Subdivisible wc)
  => Wiggler
  -> WaterColourLayering
  -> wc
  -> Generate [wc]
waterColour wiggler (WaterColourLayering layerCount opacity depthOfBranch depthPerBranch) src = do
  base <- warpN wiggler depthOfBranch src
  layers <-
    sequence $ map (const $ warpN wiggler depthPerBranch base) [1 .. layerCount]
  return $ map (setOpacity opacity) layers

radialWiggler :: Double -> Wiggler
radialWiggler power =
  Wiggler $ \p -> do
    theta <- sampleRVar $ uniform 0 (2 * pi)
    r <- sampleRVar $ normal 0 power
    return $ circumPoint p theta r

instance Wiggle BezierControlPoints where
  wiggle (Wiggler wiggleF) (BezierControlPoints start cp1 cp2 end) = do
    cp1' <- wiggleF cp1
    cp2' <- wiggleF cp2
    return $ BezierControlPoints start cp1' cp2' end

data Petal = Petal
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
  let orient = rotateAbout root theta
  let left = orient $ (V2 (rx - size / 2) (ry - size))
  let right = orient $ V2 (rx + size / 2) (ry - size)
  let curve = mkCompositeCurve $ mkBezierCurve2d root left right root
  let curve' = subdivideN 2 curve
  wigglePower <- sampleRVar (normal 0 (size / 20)) >>= return . abs
  let wiggler = radialWiggler wigglePower
  curve'' <- sequence $ map (wiggle wiggler) $ realizeCurve curve'
  colour <- assignTHColour palette root
  return $ Petal root size curve'' (colour, 1)

circleBezierFactor = 0.551915024494

circleCurve :: Double -> V2 Double -> CompositeCurve
circleCurve scale root@(V2 x y) =
  let offset = scale * circleBezierFactor
      left@(V2 lx ly) = root + V2 (negate scale) 0
      right@(V2 rx ry) = root + V2 scale 0
      top@(V2 tx ty) = root + V2 0 (negate scale)
      bottom@(V2 bx by) = root + V2 0 scale
      seg1 =
        mkBezierCurve2d top (V2 (tx + offset) ty) (V2 rx (ry - offset)) right
      seg2 =
        mkBezierCurve2d right (V2 rx (ry + offset)) (V2 (bx + offset) by) bottom
      seg3 =
        mkBezierCurve2d bottom (V2 (bx - offset) by) (V2 lx (ly + offset)) left
      seg4 =
        mkBezierCurve2d left (V2 lx (ly - offset)) (V2 (tx - offset) ty) top
   in CompositeCurve [seg1, seg2, seg3, seg4]

data Flower = Flower
  { _flowerPetals :: [Petal]
  , _flowerPetalOutlines :: [Petal]
  , _flowerCore :: [([BezierControlPoints], (RGB Double, Double))]
  }

debugControlPoints :: BezierControlPoints -> Render ()
debugControlPoints (BezierControlPoints start cp1 cp2 end) = do
  let debugPoint p = draw (Circle p 4) >> fill
  foldr1 (>>) $ map debugPoint [cp1, cp2]

mkFlower :: V2 Double -> Generate Flower
mkFlower root = do
  size <- sampleRVar $ uniform 40 100
  let core = circleCurve size root
  let core' = subdivide core
  wigglePower <- sampleRVar (normal 0 $ (size / 5)) >>= return . abs
  let wiggler = radialWiggler wigglePower
  let coreColour = RGB 0 0 0
  core'' <- sequence $ map (wiggle wiggler) $ realizeCurve core'
  let meltCore curve = sequence $ map (wiggle wiggler) curve
  meltedCore <-
    sequence $
    map
      (const $ meltCore core'' >>= \c -> return (c, (coreColour, 0.2)))
      [0 .. 30]
  petalCount :: Int <- sampleRVar $ uniform 10 100
  petalThetas <-
    sequence $ map (const $ sampleRVar $ uniform 0 (2 * pi)) [0 .. petalCount]
  thMote <- mkTHColours mote
  let makePetals = do
        raw <- sequence $ map (mkPetal thMote (size * 5) root) petalThetas
        melted :: [[Petal]] <-
          sequence (map (flatWaterColour 0.01 30 wiggler) raw)
        return $ concat melted
  petalOutlines <- makePetals
  petals <- makePetals
  return $ Flower petals petalOutlines meltedCore

instance Drawable Flower where
  draw (Flower petals petalOutlines core) = do
    let drawPetal (petal, outline) = do
          setSourceRGBA 1 1 0 1
          draw petal >> fill
          setSourceRGBA 0 0 0 1
          draw outline >> stroke
    foldr (>>) (pure ()) $ map drawPetal $ zip petals petalOutlines
    let drawCore (curve, col) = do
          setColour col
          draw curve
          closePath
          fill
    --foldr (>>) (pure ()) $ map (drawCore) core
    return ()

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
