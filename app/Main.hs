module Main where

import qualified Data.Vector as V
import Linear hiding (rotate)
import System.IO.Unsafe

import Control.Exception
import Debug.Trace as D
import Generate
import qualified Generate.Algo.CirclePack as P
import qualified Generate.Algo.QuadTree as Q
import qualified Generate.Algo.Vec as V
import Generate.Collision.Bounds
import Generate.Colour.SimplePalette
import Generate.Colour.THColours
import Generate.Geom.Frame
import Generate.Patterns.Grid
import Generate.Patterns.Maze
import Generate.Patterns.Noise
import Generate.Patterns.RecursiveSplit
import Generate.Patterns.Sampling
import Generate.Patterns.Splatter
import Generate.Patterns.Tangles
import Generate.Patterns.Walker
import Generate.Patterns.Water
import Generate.Patterns.Wiggle
import Generate.Transforms.Warp
import qualified Streaming as S
import qualified Streaming.Prelude as S
import System.IO.Unsafe

ramp :: Int -> [Double]
ramp total = map valueOf [0 .. total]
  where
    valueOf i = (fromIntegral i) / (fromIntegral total)

mkPalette :: Generate SimplePalette
mkPalette =
  randElem $
  V.fromList
    [ mkSimplePalette
        "030303"
        ["68A793", "ECBF1F", "E2B01A", "B95928", "8F253F"]
    , mkSimplePalette "211721" ["4A294D", "F3237F", "DC5956", "F383D0"]
    , mkSimplePalette "EFC271" ["3E8A79", "E9A931", "F03E4D", "CC3433"]
    , mkSimplePalette "A35A49" ["35322B", "FDC8D0", "E69A9A", "FCFEFD"]
    ]

background :: SimplePalette -> Generate (Render ())
background palette = do
  World {..} <- asks world
  return $ do
    setColour $ bgColour palette
    rectangle 0 0 width height
    fill

bgStream :: Stream (Render ())
bgStream = do
  palette <- lift $ mkPalette
  render <- lift $ background palette
  S.yield render

data State =
  State
    { palette :: SimplePalette
    , thColours :: THColours
    , pathOrigin :: V2 Double
    , wiggler :: Wiggler
    }

start :: Generate State
start = do
  palette <- mkPalette
  thColours <- mkTHColours palette
  pathOrigin <- centerPoint
  strength <- sampleRVar $ uniform 0 40
  scale <- sampleRVar $ uniform 10 200
  let wiggler = mkNoiseWiggler 100 strength scale
  return $
    State
      { palette = palette
      , thColours = thColours
      , pathOrigin = pathOrigin
      , wiggler = wiggler
      }

data Mountain =
  Mountain
    { palette :: SimplePalette
    , path :: Stream (V2 Double)
    }

instance Subdivisible Mountain where
  subdivide m@(Mountain {..}) = m {path = subdivide path}

instance Element Mountain where
  realize (Mountain {..}) = do
    c <- fgColour palette
    path' <- realize path
    return $ do
      setColour c
      path'
      fill

instance Wiggle Mountain where
  wiggle wiggler m@(Mountain {..}) =
    pure $ (m {path = S.mapM (wiggle wiggler) path} :: Mountain)

mkMountain :: State -> Generate Mountain
mkMountain State {..} = do
  World {..} <- asks world
  mountainWidth <- sampleRVar $ uniform (width / 10) (width / 2)
  mountainHeight <- sampleRVar $ uniform (height / 10) (height / 5)
  offset <- sampleRVar $ uniform (width / 10) (width - width / 10)
  V2 _ y <- centerPoint
  let middleX = offset + (mountainWidth / 2)
  return $
    Mountain palette $
    S.each
      [ V2 offset y
      , V2 middleX (y - mountainHeight)
      , V2 (offset + mountainWidth) y
      ]

data WaveBand =
  WaveBand
    { palette :: THColours
    , y :: Double
    , path :: Stream (V2 Double)
    }

instance Subdivisible WaveBand where
  subdivide w@(WaveBand {..}) = w {path = subdivide path}

mkWaveBand :: THColours -> Double -> Double -> WaveBand
mkWaveBand palette tall y =
  WaveBand palette y $ do
    World {..} <- lift $ asks world
    subdivideN 4 $ S.each [V2 0 $ y + tall, V2 width $ y + tall]

applyWave :: Wiggler -> WaveBand -> WaveBand
applyWave wiggler w@(WaveBand {..}) = w {path = S.mapM wigglePoint path}
  where
    wigglePoint p@(V2 x y) = do
      V2 _ y' <- wiggle wiggler p
      return $ V2 x $ max y y'

instance Element WaveBand where
  realize (WaveBand {..}) = do
    path' <- S.toList_ path
    c <- assignTHColour palette $ V2 0 y
    World {..} <- asks world
    return $ do
      setColour c
      draw $ V2 width y : V2 0 y : path'
      closePath
      fill

waveBands :: State -> Generate (Stream (Render ()))
waveBands State {..} = do
  World {..} <- asks world
  bandCount :: Int <- sampleRVar $ uniform 3 100
  let bandHeight = height / fromIntegral bandCount
  let waveBands =
        map (mkWaveBand thColours (bandHeight + 1) . (* height)) $
        ramp bandCount
  let waveBands' = reverse $ map (applyWave wiggler) waveBands
  return $ realizeAll waveBands'

defaultWiggler :: Generate Wiggler
defaultWiggler = do
  z <- sampleRVar $ uniform 0 1000
  strength <- sampleRVar $ normal 0 30
  smoothness <- sampleRVar $ normal 200 40
  return $ mkNoiseWiggler z strength smoothness

data Dot =
  Dot
    { palette :: SimplePalette
    , thickness :: Double
    , center :: V2 Double
    }

instance Element Dot where
  realize (Dot palette thickness center) = do
    col <- fgColour palette
    return $ do
      setColour col
      draw $ Circle center thickness
      fill

dots :: SimplePalette -> Rect -> Double -> Generate (Render ())
dots palette (Rect topLeft width height) thickness = do
  scale <- sampleRVar $ uniform 15 23
  ps <-
    grid
      def
        { cols = scale
        , rows = scale
        , topLeft = topLeft
        , width = Just width
        , height = Just height
        }
  let dots = map (Dot palette thickness) ps
  dots' <- mapM realize dots
  return $ foldr1 (>>) dots'

data Rorshock d =
  Rorshock
    { state :: State
    , drawable :: d
    }

data Outline c d =
  Outline c Double d

instance (CairoColour c, Drawable d) => Element (Outline c d) where
  realize (Outline c w d) =
    return $ setColour c >> setLineWidth w >> draw d >> stroke

data Solid d =
  Solid d

instance (Drawable d) => Element (Solid d) where
  realize (Solid d) = return $ draw d >> fill

data Matte d =
  Matte
    { matte :: Generate (Render ())
    , drawable :: d
    }

instance (Drawable d) => Element (Matte d) where
  realize (Matte matte drawable) =
    matte >>= return . alphaMatte (draw drawable >> fill)

data Dotted =
  Dotted SimplePalette [V2 Double]

instance Element Dotted where
  realize (Dotted palette line) = do
    thickness <- sampleRVar $ normal 0.1 0.02 >>= return . abs
    col <- fgColour palette
    fill <- realize $ Outline col thickness line
    dotSize <- sampleRVar $ normal 1.0 0.4 >>= return . abs
    dots <- mapM (realize . Dot palette dotSize) line
    return $ foldr1 (>>) $ fill : dots

instance (Points d, Drawable d, BoundingRect d) => Element (Rorshock d) where
  realize (Rorshock state@(State {..}) drawable) = do
    strokeThickness <- sampleRVar (normal 0.2 0.1) >>= return . abs
    rawBands <- waveBands state
    colour <- fgColour palette
    let bands = S.fold_ (>>) (pure ()) id rawBands
    let base = do
          setColour colour
          draw drawable
          closePath
    thickness <- sampleRVar $ normal 0.8 0.4 >>= return . abs
    let bounds = boundingRect drawable
    let dotMatte = dots palette bounds thickness
    realizer <-
      randElem $
      V.fromList
        [ realize . Outline colour strokeThickness
        , realize . Solid
        , realize . Matte bands
        , realize . Matte dotMatte
        , \_ -> realize $ Dotted palette (points drawable)
        ]
    realizer base

repeatStream :: Stream a -> Stream a
repeatStream s = do
  first <- lift $ S.next s
  case first of
    Left _ -> repeatStream s
    Right (first, _) ->
      let step as = do
            next <- S.next as
            case next of
              Left _ -> S.next $ repeatStream s
              Right (next, rest) -> return $ Right (next, rest)
       in S.unfoldr step s

walkDebug ::
     State
  -> V2 Double
  -> Generate Double
  -> Double
  -> Generate (Stream (Render ()))
walkDebug state@(State {..}) origin mkSize radius = do
  let splotchFromPoint p = do
        size <- mkSize
        let base = ngon 0 8 size p
        let transforms = S.iterateM transform (pure base)
        let depth = 2
        (S.head_ $ S.drop (depth - 1) transforms) >>= return . fromJust
  let cfg = symSplatter $ normal 0 radius
  let circles = S.mapM splotchFromPoint $ sampleStream $ mkSplatter cfg origin
  return $ S.mapM realize $ S.map (Rorshock state) circles
  where
    transform :: Shape -> Generate Shape
    transform shape =
      warp def shape >>= return . subdivide >>= warp def >>= warp def

data Pillar =
  Pillar
    { state :: State
    , top :: V2 Double
    , pillarWidth :: Double
    }

instance Element Pillar where
  realize (Pillar {state = State {..}, ..}) = do
    col <- fgColour palette
    World {..} <- asks world
    let V2 x _ = top
    let line = [top, V2 x height]
    let line' = V.fromList $ subdivideN 5 line
    let completions = V.map (^ 3) $ V.fromList $ ramp $ V.length line'
    let drift = 100
    let line'' =
          V.map (\(c, (V2 x y)) -> V2 (x - c * drift) y) $
          V.zip completions line'
    return $ do
      setColour col
      setLineCap LineCapRound
      setLineWidth pillarWidth
      draw $ V.toList line''
      stroke

mkPillars :: State -> Generate (Stream Pillar)
mkPillars state@(State {..}) = do
  World {..} <- asks world
  spireWidth <- sampleRVar (uniform 5 40) >>= return . (width /)
  basePillarWidth <- sampleRVar (uniform 3 80) >>= return . (spireWidth /)
  spacing <- sampleRVar (uniform 0.1 0.5) >>= return . (basePillarWidth /)
  V2 centerX baseY <- centerPoint
  baseYShift <- sampleRVar $ uniform 0 (height / 15)
  let mkPillar x = do
        y <- sampleRVar $ normal (baseY - baseYShift) $ height / 30
        w <- sampleRVar $ normal basePillarWidth $ basePillarWidth / 5
        return $ (Pillar state (V2 x y) w, x + w + spacing)
  let nextPillar (_, x) = mkPillar x
  let startX = centerX - (spireWidth / 2)
  let endX = centerX + (spireWidth / 2)
  return $
    S.map fst $
    S.takeWhile ((< endX) . snd) $ S.iterateM nextPillar $ mkPillar startX

eruptPillar :: Pillar -> Stream (Render ())
eruptPillar p@(Pillar {..}) = do
  let prefix :: Stream (Render ()) = S.mapM realize $ S.each [p]
  let size = fmap abs $ sampleRVar $ normal (pillarWidth * 20) (pillarWidth)
  let radius = pillarWidth * 4
  eruptions <-
    lift $ sampleRVar $ fmap (floor . abs) $ normal (4 :: Double) (2 :: Double)
  walk :: Stream (Render ()) <- lift $ walkDebug state top size radius
  prefix >> S.take eruptions walk

mkStars :: State -> Generate (Stream (Render ()))
mkStars state@(State {..}) = do
  let size = fmap abs $ sampleRVar $ normal 80 30
  let walkFrom origin = do
        walk <- walkDebug state origin size 20
        fmap fromJust $ S.head_ walk
  frame <- fullFrame
  let points = sampleStream frame
  return $ S.mapM walkFrom points

sketch :: State -> Generate (Stream (Render ()))
sketch state@(State {..}) = do
  pillars <- mkPillars state
  let eruptions = concatStreams $ S.map eruptPillar pillars
  stars <- mkStars state
  return $ streamGenerates [background palette] >> eruptions >> S.take 30 stars

main :: IO ()
main = do
  runStatefulInvocation start sketch return
