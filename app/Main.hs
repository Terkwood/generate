module Main where

import qualified Data.Vector as V
import Linear hiding (rotate)

import Control.Exception
import Debug.Trace as D
import Generate
import Generate.Patterns.Grid
import qualified Streaming as S
import qualified Streaming.Prelude as S

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
    , signalPalette :: SignalPalette
    }

start :: Generate State
start = do
  palette <- mkPalette
  let [a, b, c, d] =
        [V3 0.5 0.5 0.5, V3 0.5 0.5 0.5, V3 2.0 1.0 0.0, V3 0.5 0.2 0.25]
  let signalPalette = mkCosinePalette a b c d
  return $ State palette signalPalette

gBlocks :: State -> Generate (Stream (Render ()))
gBlocks state@(State {..}) = do
  World {..} <- asks world
  blocks <- tiles def {rows = 300, cols = 200}
  let signal x y =
        sin ((pi / 2) * (x / width / 2) + (fromIntegral $ floor x `div` 40)) +
        y / height
  let colourBlock block@(Rect (V2 x y) _ _) = do
        setColour $ signalColour signalPalette $ (signal x y)
        draw block
        fill
  return $ do S.map colourBlock $ S.each blocks

sketch :: State -> Generate (Stream (Render ()))
sketch state@(State {..}) = do
  blocks <- gBlocks state
  return $ streamGenerates [background palette] >> blocks

main :: IO ()
main = do
  runStatefulInvocation start sketch return
