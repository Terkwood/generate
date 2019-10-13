module Generate.Algo.Ramp
  ( mkRamp
  , mkEndlessRamp
  ) where

mkRamp :: Int -> [Double]
mkRamp n = take n $ mkEndlessRamp n

mkEndlessRamp :: Int -> [Double]
mkEndlessRamp total = map valueOf [0 ..]
  where
    valueOf i = (fromIntegral i) / (fromIntegral total)
