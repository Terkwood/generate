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
import Generate.Colour.SimplePalette
import Generate.Colour.THColours
import Generate.Geom.Frame
import Generate.Patterns.Grid
import Generate.Patterns.Maze
import Generate.Patterns.RecursiveSplit
import Generate.Patterns.Sampling
import Generate.Patterns.Water
import Generate.Patterns.Wiggle
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

data Step =
  Step
    { stepIdx :: Int
    , theta :: Double
    , position :: V2 Double
    , turnTheta :: Double
    }

data Walker =
  Walker
    { agility :: Generate Double
    , step :: Double
    }

gaussianWalker :: Double -> Walker
gaussianWalker strength =
  Walker {agility = sampleRVar $ normal 0 strength, step = 0.4}

walk :: Walker -> V2 Double -> Stream Step
walk walker origin =
  let first = do
        theta <- sampleRVar $ uniform 0 (2 * pi)
        return $ Step 0 theta origin pi
   in S.iterateM (_step walker) first

_step :: Walker -> Step -> Generate Step
_step (Walker {..}) (Step {..}) = do
  turn <- agility
  let (turnTheta', turn') =
        if theta `mod'` (2 * pi) > turnTheta
          then ((turnTheta + pi) `mod'` (2 * pi), turn)
          else (turnTheta, turn' * 2)
  let theta' = theta + turn
  let next = circumPoint position theta step
  return $ Step (stepIdx + 1) theta' next turnTheta'

intersectionPolies :: Stream Step -> Generate [Line]
intersectionPolies steps = do
  steps <- S.toList_ steps
  frame <- fullFrame
  let polies =
        catMaybes $
        found $
        foldr
          (search $ V.fromList $ map position steps)
          (PolySearch [] (Q.new frame) 0 Nothing)
          steps
  return polies

defaultWiggler :: Generate Wiggler
defaultWiggler = do
  z <- sampleRVar $ uniform 0 1000
  strength <- sampleRVar $ normal 0 30
  smoothness <- sampleRVar $ normal 200 40
  return $ mkNoiseWiggler z strength smoothness

data PolySearch =
  PolySearch
    { found :: [Maybe Line]
    , tree :: Q.QuadTree Int
    , idx :: Int
    , lastIntersection :: Maybe Int
    }

search :: V.Vector (V2 Double) -> Step -> PolySearch -> PolySearch
search vs step@(Step {..}) (PolySearch {..}) =
  let leaf = Q.Leaf position idx
      (is, tree') = Q.insert tree leaf
      intersection = do
        (Q.Leaf np j) <- Q.nearest tree leaf
        if distance np position >= 0.05
          then Nothing
          else Just j
      line = do
        begin <- intersection
        let slice = V.slice begin (idx - begin) vs
        mkLine slice
   in PolySearch
        { found = line : found
        , tree = tree'
        , idx = idx + 1
        , lastIntersection = intersection
        }

data Dot =
  Dot
    { palette :: SimplePalette
    , center :: V2 Double
    }

instance Element Dot where
  realize (Dot palette center) = do
    col <- fgColour palette
    return $ do
      setColour col
      draw $ Circle center 1
      fill

dots :: SimplePalette -> Generate (Render ())
dots palette = do
  scale <- sampleRVar $ uniform 100 1000
  ps <- grid def {cols = scale, rows = scale}
  let dots = map (Dot palette) ps
  dots' <- mapM realize dots
  return $ foldr1 (>>) dots'

data Rorshock =
  Rorshock
    { state :: State
    , splotch :: Splotch
    }

instance Element Rorshock where
  realize (Rorshock state@(State {..}) splotch) = do
    thickness <- sampleRVar (normal 0.2 0.1) >>= return . abs
    rawBands <- waveBands state
    bands <- S.fold_ (>>) (pure ()) id rawBands
    mode :: Double <- sampleRVar $ uniform 0 1
    let base = do
          setLineWidth thickness
          draw splotch
    dotMatte <- dots palette
    return $
      if mode < 0.3
        then if mode < 1.5
               then alphaMatte (base >> fill) bands
               else alphaMatte dotMatte (base >> fill)
        else base >> stroke

sketch :: State -> Generate (Stream (Render ()))
sketch state@(State {..}) = do
  count <- sampleRVar $ uniform 1 100
  return $
    streamGenerates [background palette] >>
    S.concat (S.take count $ S.repeatM shapes)
  where
    shapes :: Generate [Render ()] = do
      let inFrame (Step {..}) = do
            frame <- fullFrame
            return $ withinRect frame position
      polies :: [Line] <-
        intersectionPolies $ S.take 4000 $ S.takeWhileM inFrame steps
      let toSplotches line = do
            col <- assignTHColour thColours $ V.head $ toVertices line
            let splotch = mkSplotch line col
            bg <-
              flatWaterColour 1 2 (const $ defaultWiggler) $
              subdivideN 2 splotch
            return $ bg
      splotches <- concatMapM toSplotches polies
      mapM (realize . Rorshock state) splotches
    steps = walk (gaussianWalker 0.3) pathOrigin

main :: IO ()
main = do
  runStatefulInvocation start sketch return
