module Main where

import Data.Array.Repa.IO.BMP (writeImageToBMP)

import Draw
import Img
import Rand
import Shape

main :: IO ()
main =
  let (poly, _) =
        (iterate (\(poly, dist) -> warp dist poly) $
         ( Irregular
             [ Edge (Point (0, 0), Point (100, 0))
             , Edge (Point (100, 0), Point (100, 100))
             , Edge (Point (100, 100), Point (0, 200))
             , Edge (Point (0, 200), Point (0, 0))
             ]
         , (gaussianBySeed 5 50))) !!
        7
  in writeImageToBMP "out.bmp" $
     rasterLayer $ draw (1, 1, 0, 1) (fillLayer (0, 1, 0, 1) $ newLayer (500, 500)) poly
