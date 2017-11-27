{-# LANGUAGE NamedFieldPuns #-}

module Main where

import Data.Array.Repa.IO.BMP (writeImageToBMP)
import qualified Data.Vector as V

import Color
import Color.Shaders
import Coords (Point(..))
import Patterns
import Poly
import Poly.Shapes
import Rand
import Raster (render)
import Raster.Mask (rasterMasks)
import Raster.Poly.Scan
import Transformers.WaterColor

main :: IO ()
main = writeImageToBMP "new.bmp" $ render $ rasterMasks (standardBlender) masks
  where
    masks = V.map (uncurry scanRaster) waterColors
    waterColors =
      waterColor 10 30 5 0.5 (shader) $ square Point {x = 0.1, y = 0.1} 0.3
    shader = staticFill RGBA {red = 1, green = 0, blue = 0, alpha = 1}
