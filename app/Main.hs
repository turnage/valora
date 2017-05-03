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
    rect = Rect {topLeft = Point {x = 400, y = 400}, bottomRight = Point {x = 200, y = 300}}
    yellowTriangles = map (\t -> ((1, 1, 0, 0.8), t)) warpedTriangles
    (warper', warpedTriangles) = warpDupe warper 6 30 triangle
    warper = Warper {dist = gaussianBySeed 90 100, heat = heatMap}
    (width, height) = (500, 300)
    ratio n d = (fromIntegral n) / (fromIntegral d)
    heatMap (Point {x = x, y = y}) = (ratio x width) * (ratio y height)
    bg = fillLayer (1, 1, 1, 1) $ newLayer (500, 500)
    triangle =
      Irregular
        [ Edge {start = Point {x = 200, y = 300}, end = Point {x = 300, y = 300}}
        , Edge {start = Point {x = 300, y = 300}, end = Point {x = 250, y = 200}}
        , Edge {start = Point {x = 250, y = 200}, end = Point {x = 200, y = 300}}
        ]
