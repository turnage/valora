{-# LANGUAGE NamedFieldPuns #-}

module Main where

import Data.Array.Repa.IO.BMP (writeImageToBMP)

import Color
import qualified Data.Vector as V
import Patterns
import Poly
import Raster (Rasterable(..), render)
import Scan

main :: IO ()
main = writeImageToBMP "new.bmp" $ render $ raster 500 outScan
  where
    outScan =
      scan (standardBlender) $ V.fromList [preprocessPoly tileColor square]
    tileColor = RGBA {red = 1, green = 0, blue = 0, alpha = 1}
    square = Square {bottomLeft = Point {x = 0.1, y = 0.1}, size = 0.3}
