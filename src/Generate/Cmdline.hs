module Generate.Cmdline
  ( MainOptions(..)
  ) where

import Options

data MainOptions =
  MainOptions
    { optWidth :: Double
    , optHeight :: Double
    , optSave :: String
    , optScale :: Double
    , optSeed :: Int
    , optFrames :: Int
    , optBrainstorm :: Bool
    }

instance Options MainOptions where
  defineOptions =
    pure MainOptions <*> simpleOption "w" 1000 "Piece width." <*>
    simpleOption "h" 1272 "Piece height." <*>
    simpleOption "o" "" "Save location." <*>
    simpleOption "s" 1 "Scale factor." <*>
    simpleOption "e" 0 "Rng seed." <*>
    simpleOption "f" 1 "Number of frames to save to file." <*>
    simpleOption "b" False "Brainstorm mode."
