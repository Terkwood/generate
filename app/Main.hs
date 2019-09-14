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
import Petal
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
    [ mondrian
    , mkSimplePalette "EFC271" ["3E8A79", "E9A931", "F03E4D", "CC3433"]
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
    }

start :: Generate State
start = do
  palette <- mkPalette
  thColours <- mkTHColours palette
  pathOrigin <- centerPoint
  return $
    State {palette = palette, thColours = thColours, pathOrigin = pathOrigin}

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
        begin <- lastIntersection
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
    { center :: V2 Double
    }

instance Element Dot where
  realize (Dot c) = do
    return $ do
      setSourceRGB 0 0 0
      draw $ Circle c 1
      fill

mkNoiseWiggler :: Double -> Double -> Double -> Wiggler
mkNoiseWiggler z strength smoothness =
  Wiggler $ \p@(V2 x y) -> do
    let scale = 1 / smoothness
    let fixSamplePoint = fmap (scale *)
    theta <-
      (noiseSample $ fixSamplePoint $ V3 x y z) >>= return . (\x -> x * 2 * pi)
    r <-
      (noiseSample $ fixSamplePoint $ V3 x y (negate z)) >>=
      return . (* strength)
    return $ circumPoint p theta r

defaultWiggler :: Generate Wiggler
defaultWiggler = do
  z <- sampleRVar $ uniform 0 1000
  strength <- sampleRVar $ normal 0 30
  smoothness <- sampleRVar $ normal 200 40
  return $ mkNoiseWiggler z strength smoothness

data Stripe =
  Stripe
    { palette :: THColours
    , line :: Line
    }

instance Element Stripe where
  realize (Stripe palette line) = do
    col <- assignTHColour palette $ V.head $ toVertices line
    return $ do
      setLineWidth 0.2
      setColour (col, 0.1 :: Double)
      draw line
      stroke

stripes :: THColours -> Stream Stripe
stripes palette =
  S.mapM pointToStripe $ unfoldGenerates $ grid def {rows = 100, cols = 100}
  where
    pointToStripe p = do
      theta <- sampleRVar $ normal (pi / 4) (pi / 12)
      length <- sampleRVar $ normal 5 1
      let end = circumPoint p theta length
      return $ Stripe palette $ fromJust $ mkLine $ V.fromList [p, end]

data Rorshock =
  Rorshock
    { splotch :: Splotch
    }

instance Element Rorshock where
  realize (Rorshock splotch) = do
    thickness <- sampleRVar (normal 0.2 0.1) >>= return . abs
    mode :: Double <- sampleRVar $ uniform 0 1
    let finisher =
          if mode < 0.2
            then fill
            else stroke
    return $ do
      setLineWidth thickness
      draw splotch
      finisher

sketch :: State -> Stream (Render ())
sketch state@(State {..}) =
  streamGenerates [background palette] >> S.concat (S.take 1 $ S.repeatM shapes)
    {-trace = do
      ps <- S.toList_ $ S.take stepCount $ S.map position steps
      return $ (\l -> draw l >> stroke) $ fromJust $ mkLine $ V.fromList ps-}
  where
    shapes :: Generate [Render ()] = do
      let inFrame (Step {..}) = do
            frame <- fullFrame
            return $ withinRect frame position
      polies :: [Line] <- intersectionPolies $ S.takeWhileM inFrame steps
      let toSplotches line = do
            col <- assignTHColour thColours $ V.head $ toVertices line
            let splotch = mkSplotch line col
            bg <-
              flatWaterColour 0.04 100 (const $ defaultWiggler) $
              subdivideN 2 splotch
            return $ bg
      splotches <- concatMapM toSplotches polies
      mapM (realize . Rorshock) splotches
    steps = walk (gaussianWalker 0.3) pathOrigin

main :: IO ()
main = do
  runStatefulInvocation start sketch return
