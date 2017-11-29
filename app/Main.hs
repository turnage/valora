{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}

module Main where

import Data.Array.Repa.IO.BMP (writeImageToBMP)
import qualified Data.Vector as V
import System.Random

import Color (RGBA(..))
import Color (standardBlender)
import Color.Shaders (Shader(..), staticFill)
import Control.Monad.Random (runRand)
import Coords (Point(..))
import Generators.WaterColor
import Patterns.Sparkles (sparkles)
import Poly.Shapes (ngon)
import Raster (render, rasterWith, emptyRaster)
import Raster.Mask (rasterMasks)
import Raster.Poly.Scan (scanRaster)

main :: IO ()
main = writeImageToBMP "new.bmp" $ render preraster
  where
    preraster = rasterMasks (standardBlender) rasters
    rasters = V.map (uncurry scanRaster) triangles
    triangles = V.map ((shader, ) . (ngon 0.1 3)) triangleSpawners
    (triangleSpawners, _) = runRand (sparkles 100) $ mkStdGen 11
    shader = staticFill RGBA {red = 1, blue = 0, green = 0, alpha = 1}
