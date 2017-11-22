{-# LANGUAGE NamedFieldPuns #-}

module Main where

import Data.Array.Repa.IO.BMP (writeImageToBMP)
import qualified Data.Vector as V

import Color
import Color.Shaders
import Patterns
import Poly
import Poly.Shapes
import Rand
import Raster (render)
import Raster.Poly.Scan
import Transformers.Warp

main :: IO ()
main = writeImageToBMP "new.bmp" $ render $ tileRaster
  where
    tileRaster = scanRaster (standardBlender) $ V.fromList [(tileShader, sq)]
    tileShader = staticFill RGBA {red = 1, green = 0, blue = 0, alpha = 1}
    sq = V.head $ warp 4 0.1 rng $ square Point {x = 0.1, y = 0.1} 0.3
    rng = randFeed $ gaussianBySeed 11 0.1
