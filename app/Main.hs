module Main where

import Data.Array.Repa.IO.BMP (writeImageToBMP)

import Draw
import Img
import Rand
import Shape
import WaterColor

main :: IO ()
main =
  let (poly, _) =
        (iterate (\(poly, dist) -> warp dist poly) $
         ( Irregular
             [ Edge (Point (200, 300), Point (300, 300))
             , Edge (Point (300, 300), Point (250, 200))
             , Edge (Point (250, 200), Point (200, 300))
             ]
         , (gaussianBySeed 85 100))) !!
        11
  in writeImageToBMP "out.bmp" $ rasterLayer $ fromMap (500, 500) $ draw (1, 1, 0, 1) poly
