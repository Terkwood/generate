module Generate.Geom.Spline
  ( Spline2d
  , BezierCurve2d
  , CompositeCurve(..)
  , BezierControlPoints(..)
  , mkSpline2d
  , sampleSpline
  , mkBezierCurve2d
  , mkCompositeCurve
  , realizeCurve
  ) where

import Control.Lens
import qualified Data.Vector as V
import Graphics.Rendering.Cairo
import Linear
import Math.Spline
import Math.Spline.BSpline
import Math.Spline.BezierCurve
import Math.Spline.Knots

import Generate.Draw
import Generate.Geom

data BezierControlPoints =
  BezierControlPoints (V2 Double)
                      (V2 Double)
                      (V2 Double)
                      (V2 Double)

instance Drawable BezierControlPoints where
  draw (BezierControlPoints (V2 sx sy) (V2 cp1x cp1y) (V2 cp2x cp2y) (V2 ex ey)) = do
    moveTo sx sy
    curveTo cp1x cp1y cp2x cp2y ex ey

data BezierSegment =
  BezierSegment BezierControlPoints

instance Drawable BezierSegment where
  draw (BezierSegment (BezierControlPoints _ (V2 cp1x cp1y) (V2 cp2x cp2y) (V2 ex ey))) = do
    curveTo cp1x cp1y cp2x cp2y ex ey

instance Drawable [BezierControlPoints] where
  draw [] = pure ()
  draw (head:rest) = do
    draw head
    drawAll $ map BezierSegment rest

data CompositeCurve =
  CompositeCurve [BezierCurve2d]

instance Drawable CompositeCurve where
  draw curve = draw $ realizeCurve curve

mkCompositeCurve :: BezierCurve2d -> CompositeCurve
mkCompositeCurve curve = CompositeCurve [curve]

realizeCurve :: CompositeCurve -> [BezierControlPoints]
realizeCurve (CompositeCurve curves) = map (bezierControlPoints) curves

data BezierCurve2d = BezierCurve2d
  { _bezierCurve2dxCurve :: BezierCurve Double
  , _bezierCurve2dyCurve :: BezierCurve Double
  } deriving (Show)

mkBezierCurve2d ::
     V2 Double -> V2 Double -> V2 Double -> V2 Double -> BezierCurve2d
mkBezierCurve2d start cp1 cp2 end = BezierCurve2d xCurve yCurve
  where
    cps = V.fromList [start, cp1, cp2, end]
    xCurve = bezierCurve $ V.map (^. _x) cps
    yCurve = bezierCurve $ V.map (^. _y) cps

bezierControlPoints :: BezierCurve2d -> BezierControlPoints
bezierControlPoints (BezierCurve2d xCurve yCurve) =
  BezierControlPoints cp0 cp1 cp2 cp3
  where
    cp0:cp1:cp2:cp3:_ = zipWith (V2) xs ys
    xs = V.toList $ controlPoints xCurve
    ys = V.toList $ controlPoints yCurve

splitCurve :: BezierCurve2d -> (BezierCurve2d, BezierCurve2d)
splitCurve (BezierCurve2d xCurve yCurve) =
  (BezierCurve2d leftX leftY, BezierCurve2d rightX rightY)
  where
    (leftX, rightX) = splitBezierCurve xCurve 0.5
    (leftY, rightY) = splitBezierCurve yCurve 0.5

instance Subdivisible CompositeCurve where
  subdivide (CompositeCurve curves) = CompositeCurve $ go [] curves
    where
      go :: [BezierCurve2d] -> [BezierCurve2d] -> [BezierCurve2d]
      go finished [] = finished
      go finished (next:rest) =
        let (left, right) = splitCurve next
         in go (finished ++ [left, right]) rest

data Spline2d = Spline2d
  { xSpline :: BSpline V.Vector Double
  , ySpline :: BSpline V.Vector Double
  , controlCount :: Int
  } deriving (Show)

extend :: Int -> V.Vector a -> V.Vector a
extend n vs =
  V.concat [V.replicate n $ V.head vs, vs, V.replicate n $ V.last vs]

mkSpline2d :: V.Vector (V2 Double) -> Maybe Spline2d
mkSpline2d verts
  | V.length verts >= 4 = Just $ Spline2d xSpline ySpline n
  | V.length verts < 4 = Nothing
  where
    knots =
      mkKnots $
      V.toList $
      extend ((n `div` 2) + 1) $ V.fromList [0.0, fromIntegral n - 1.0]
    xSpline = bSpline knots xs
    ySpline = bSpline knots ys
    xs = V.map (\(V2 x _) -> x) verts
    ys = V.map (\(V2 _ y) -> y) verts
    n = V.length verts

sampleSpline :: Spline2d -> Double -> V2 Double
sampleSpline (Spline2d xSpline ySpline _) t = V2 x y
  where
    x = evalBSpline xSpline t
    y = evalBSpline ySpline t
