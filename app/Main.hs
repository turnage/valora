module Main where

import Data.Array.Repa.IO.BMP (writeImageToBMP)

import Draw
import Img
import Rand
import Shape

main :: IO ()
main =
  let (poly, _) =
        warp (gaussianBySeed 5 0.5) $ PointPoly [Point (200, 200), Point (200, 400), Point (100, 300)]
  in writeImageToBMP "out.bmp" $
     rasterLayer $ draw (1, 1, 0, 1) (fillLayer (0, 1, 0, 1) $ newLayer (500, 500)) poly
