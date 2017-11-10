{-# LANGUAGE NamedFieldPuns #-}

module Main where

import Data.Array.Repa.IO.BMP (writeImageToBMP)

import Color
import qualified Data.Vector as V
import Patterns
import Poly
import Raster
import Scan

main :: IO ()
main = writeImageToBMP "new.bmp" $ raster outRender
  where
    outRender = render 500 (standardBlender) outScan
    outScan = scan $ V.map (preprocessPoly tileColor) $ tile 4
    tileColor = RGBA {red = 1, green = 0, blue = 0, alpha = 1}
    square = Square {topLeft = Point {x = 0.1, y = 0.1}, size = 0.3}
