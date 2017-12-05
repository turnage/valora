{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}

module Main where

import Data.Array.Repa.IO.BMP (writeImageToBMP)
import qualified Data.Vector as V
import System.Random

import Color (HSVA(..), hsva)
import Color (standardBlender)
import Color.Shaders (Shader(..), staticFill)
import Control.Monad.Random (runRand)
import Coords (Point(..))
import Generators.WaterColor
import Patterns.Sparkles (sparkles)
import Poly.Shapes (ngon)
import Poly.Translations (scale)
import Raster (render, rasterWith, emptyRaster)
import Raster.Mask (rasterMasks)
import Raster.Poly.Scan (scanRaster)

main :: IO ()
main = writeImageToBMP "new.bmp" $ render preraster
  where
    preraster = rasterMasks (standardBlender) rasters
    rasters = V.map (uncurry scanRaster) $ V.map (shader, ) triangles'
    triangles' = V.map (scale 1) triangles
    triangles = V.map (ngon 0.1 3) triangleSpawners
    (triangleSpawners, _) = runRand (sparkles 500) $ mkStdGen 11
    shader = staticFill $ hsva HSVA {hue = 256, saturation = 0.91, value = 1, alpha = 0.3}
