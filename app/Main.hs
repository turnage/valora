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
        10
  in writeImageToBMP "out.bmp" $ rasterLayer $ fromMap (500, 500) $ draw (1, 1, 0, 1) poly
