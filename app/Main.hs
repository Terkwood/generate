module Main where

import qualified Data.Vector as V
import Linear hiding (rotate)
import System.IO.Unsafe

import Generate
import qualified Generate.Algo.QuadTree as Q
import qualified Generate.Algo.Vec as V
import Generate.Colour.SimplePalette
import Generate.Patterns.Sampling

ramp :: Int -> V.Vector Double
ramp n = V.generate n $ \i -> fromIntegral i / fromIntegral (n - 1)

wiggle :: Double -> V2 Double -> Generate (V2 Double)
wiggle power p = do
  theta <- sampleRVar $ uniform 0 (2 * pi)
  r <- sampleRVar $ normal 0 power
  return $ circumPoint p theta r

wiggleBezierCurve ::
     Double -> BezierControlPoints -> Generate BezierControlPoints
wiggleBezierCurve power (BezierControlPoints start cp1 cp2 end) = do
  cp1' <- wiggle power cp1
  cp2' <- wiggle power cp2
  return $ BezierControlPoints start cp1' cp2' end

data Petal = Petal
  { _petalRoot :: V2 Double
  , _petalSize :: Double
  , _petalCurve :: [BezierControlPoints]
  }

instance Drawable Petal where
  draw (Petal _ _ curve) = draw curve

mkPetal :: Double -> V2 Double -> Double -> Generate Petal
mkPetal size root@(V2 rx ry) theta = do
  let orient = rotateAbout root theta
  let left = orient $ (V2 (rx - size / 2) (ry - size))
  let right = orient $ V2 (rx + size / 2) (ry - size)
  let curve = mkCompositeCurve $ mkBezierCurve2d root left right root
  let curve' = subdivideN 2 curve
  wigglePower <- sampleRVar (normal 0 (size / 20)) >>= return . abs
  curve'' <-
    sequence $ map (wiggleBezierCurve wigglePower) $ realizeCurve curve'
  return $ Petal root size curve''

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
  , _flowerCore :: [BezierControlPoints]
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
  core'' <- sequence $ map (wiggleBezierCurve wigglePower) $ realizeCurve core'
  petalCount :: Int <- sampleRVar $ uniform 4 10
  petalThetas <-
    sequence $ map (const $ sampleRVar $ uniform 0 (2 * pi)) [0 .. petalCount]
  petalOutlines <- sequence $ map (mkPetal (size * 5) root) petalThetas
  petals <- sequence $ map (mkPetal (size * 5) root) petalThetas
  return $ Flower petals petalOutlines $ core''

instance Drawable Flower where
  draw (Flower petals petalOutlines core) = do
    foldr (>>) (pure ()) $ map (\p -> draw p >> fill) petals
    setSourceRGBA 0 0 0 1
    foldr (>>) (pure ()) $ map (\p -> draw p >> stroke) petalOutlines
    draw core
    closePath
    fill

scene :: Generate (Render ())
scene = do
  World {..} <- asks world
  center <- centerPoint
  petalOutlineColour <- fgColour gurken
  petal <- mkFlower center
  return $ do
    setColour $ bgColour gurken
    rectangle 0 0 width height
    fill
    setColour petalOutlineColour
    draw petal
    setSourceRGBA 1 0 0 1
    setLineWidth 10.0
    return ()

main :: IO ()
main = do
  runInvocation scene
