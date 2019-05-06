module Main where

import qualified Data.Vector as V
import Linear hiding (rotate)
import System.IO.Unsafe

import Generate
import qualified Generate.Algo.QuadTree as Q
import qualified Generate.Algo.Vec as V
import Generate.Colour.SimplePalette
import Generate.Patterns.Sampling
import Math.Spline
import Math.Spline.BSpline
import Math.Spline.Knots

ramp :: Int -> V.Vector Double
ramp n = V.generate n $ \i -> fromIntegral i / fromIntegral n

extend :: Int -> V.Vector a -> V.Vector a
extend n vs =
  V.concat [V.replicate n $ V.head vs, vs, V.replicate n $ V.last vs]

data Spline2d = Spline2d
  { xSpline :: BSpline V.Vector Double
  , ySpline :: BSpline V.Vector Double
  , controlCount :: Int
  , degree :: Int
  }

bookends :: BSpline V.Vector Double -> (Double, Double)
bookends spline = (fst $ V.head knots, fst $ V.last knots)
  where
    knots = toVector $ knotVector spline

fitIn :: Double -> (Double, Double) -> Double
fitIn t (low, high) = t * (high - low) + low

sampleNormalized :: BSpline V.Vector Double -> Double -> Double
sampleNormalized spline t = evalBSpline spline $ fitIn t (bookends spline)

instance Drawable Spline2d where
  draw spline@(Spline2d xSpline ySpline _ _) = do
    let samplePoints = ramp 100
    let samples = V.map (sample spline) samplePoints
    let cps = V.zip (controlPoints xSpline) (controlPoints ySpline)
    V.foldr1 (>>) $ V.map (\p -> (draw $ Circle p 2) >> fill) samples

mkSpline2d :: Int -> Line -> Spline2d
mkSpline2d degree line = Spline2d xSpline ySpline degree n
  where
    knots = mkKnots $ V.toList $ extend degree $ V.generate n fromIntegral
    xSpline = bSpline knots xs
    ySpline = bSpline knots ys
    xs = V.map (\(V2 x _) -> x) verts
    ys = V.map (\(V2 _ y) -> y) verts
    n = V.length verts
    verts = toVertices line

multiplySpline :: Int -> Spline2d -> Spline2d
multiplySpline n spline@(Spline2d xSpline ySpline _ _) =
  spline {xSpline = multSpline xSpline, ySpline = multSpline ySpline}
  where
    multSpline spline =
      V.foldr (\k spline -> insertKnot spline k) spline toInsert
    toInsert = V.concatMap (V.replicate 2) $ V.generate n fromIntegral

toBezier :: Spline2d -> V.Vector Double
toBezier (Spline2d xSpline ySpline degree n) = undefined

sample :: Spline2d -> Double -> V2 Double
sample (Spline2d xSpline ySpline _ _) t = V2 x y
  where
    x = sampleNormalized xSpline t
    y = sampleNormalized ySpline t

data Petal = Petal
  { _petalRoot :: V2 Double
  , _petalSize :: Double
  }

instance Drawable Petal where
  draw (Petal (V2 rx ry) size) = do
    let V2 lex ley = V2 (rx - size / 2) (ry - size)
    let V2 rex rey = V2 (rx + size / 2) (ry - size)
    moveTo rx ry
    curveTo lex ley rex rey rx ry
    closePath
    stroke

scene :: Generate (Render ())
scene = do
  World {..} <- asks world
  center <- centerPoint
  let V2 cx cy = center
  petalOutlineColour <- fgColour gurken
  let spline =
        mkSpline2d 2 $
        fromJust $
        mkLine $
        V.fromList
          [center, V2 (cx - 50) (cy - 100), V2 (cx + 50) (cy - 100), center]
  return $ do
    setColour $ bgColour gurken
    rectangle 0 0 width height
    fill
    setColour petalOutlineColour
    draw $ Petal center 200
    setSourceRGBA 1 0 0 1
    draw spline
    setSourceRGBA 0 1 0 1
    draw $ multiplySpline 3 spline
    return ()

main :: IO ()
main = do
  let center@(V2 cx cy) = V2 250 250
  let spline =
        mkSpline2d 2 $
        fromJust $
        mkLine $
        V.fromList
          [center, V2 (cx - 50) (cy - 100), V2 (cx + 50) (cy - 100), center]
  putStrLn $ show $ controlPoints $ xSpline spline
  --putStrLn $ show $ toBezier spline
  runInvocation scene
