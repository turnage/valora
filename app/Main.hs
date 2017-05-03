module Main where

import Data.Array.Repa.IO.BMP (writeImageToBMP)
import Data.List (transpose)

import Draw
import Img
import Patterns
import Rand
import Shape
import Warp
import WaterColor

main :: IO ()
main =
  writeImageToBMP "out.bmp" $
  rasterLayer $ applyLayer bg $ fromMap (frameSize, frameSize) $ waterColor redRects (applyPixel)
  where
    redRects = map (\t -> ((1, 0, 0, 1), t)) $ tile frameSize 10
    bg = fillLayer (1, 1, 1, 1) $ newLayer (frameSize, frameSize)
    frameSize = 500
