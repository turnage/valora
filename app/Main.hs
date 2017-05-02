module Main where

import Data.Array.Repa.IO.BMP (writeImageToBMP)

import Draw
import Img
import Rand
import Shape
import WaterColor

main :: IO ()
main =
  let bg = fillLayer (1, 1, 1, 1) $ newLayer (500, 500)
      poly =
        Irregular
          [ Edge (Point (200, 300), Point (300, 300))
          , Edge (Point (300, 300), Point (250, 200))
          , Edge (Point (250, 200), Point (200, 300))
          ]
  in writeImageToBMP "out.bmp" $
     rasterLayer $ applyLayer bg $ fromMap (500, 500) $ waterColor 10 1000 9 100 (1, 1, 0, 1) poly
