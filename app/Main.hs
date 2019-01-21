module Main where


import           Graphics.Rendering.Cairo      as Cairo
import           Generate

scene :: Generate (Render ())
scene = return $ do
    setSourceRGB 1 0 1
    rectangle 0 0 100 100
    fill

main :: IO ()
main = runInvocation scene
