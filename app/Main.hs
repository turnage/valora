module Main where

import Data.Array.Repa.IO.BMP (writeImageToBMP)
import qualified Data.HashMap as H
import Data.List (transpose)
import Data.Traversable (mapAccumR)

import Draw
import Img
import Patterns
import Rand
import Shape
import Warp
import WaterColor

main :: IO ()
main =
  writeImageToBMP "out.bmp" $ rasterLayer $ applyLayer bg $ fromMap (frameSize, frameSize) redRects
  where
    redRects =
      H.unionsWith addPixel $
      map (\rs -> waterColor (map (\r -> ((1, 0, 0, 1), r)) rs) addPixel) rects
    (_, rects) = mapAccumR (warpDupe 7 20) warper $ map (sqTrim 20) $ tile frameSize 10
    warper = Warper {dist = gaussianBySeed 90 10, heat = (\_ -> 1)}
    bg = fillLayer (1, 1, 1, 1) $ newLayer (frameSize, frameSize)
    frameSize = 500
