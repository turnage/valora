module Main where

import Data.Array.Repa.IO.BMP (writeImageToBMP)
import Data.List (transpose)

import Draw
import Img
import Rand
import Shape
import Warp
import WaterColor

main :: IO ()
main =
  writeImageToBMP "out.bmp" $
  rasterLayer $ applyLayer bg $ fromMap (width, height) $ waterColor shapes (addPixel)
  where
    shapes = interleave redRects yellowTriangles
    interleave xs ys = concat (transpose [xs, ys])
    redRects = map (\t -> ((1, 0, 0, 0.8), t)) warpedRects
    (warper'', warpedRects) = warpDupe warper' 6 30 rect
    rect = Rect (Point (400, 400), Point (200, 300))
    yellowTriangles = map (\t -> ((1, 1, 0, 0.8), t)) warpedTriangles
    (warper', warpedTriangles) = warpDupe warper 6 30 triangle
    warper = Warper {dist = gaussianBySeed 90 100, heat = heatMap}
    (width, height) = (500, 300)
    ratio n d = (fromIntegral n) / (fromIntegral d)
    heatMap (Point (x, y)) = (ratio x width) * (ratio y height)
    bg = fillLayer (1, 1, 1, 1) $ newLayer (500, 500)
    triangle =
      Irregular
        [ Edge (Point (200, 300), Point (300, 300))
        , Edge (Point (300, 300), Point (250, 200))
        , Edge (Point (250, 200), Point (200, 300))
        ]
