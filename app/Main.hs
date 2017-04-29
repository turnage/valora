module Main where

import Data.Array.Repa.IO.BMP (writeImageToBMP)

import Draw
import Img
import Shape

main :: IO ()
main =
  writeImageToBMP "out.bmp" $
  rasterLayer $
  draw
    (1, 1, 0, 1)
    (fillLayer (0, 1, 0, 1) $ newLayer (500, 500))
    (Rect (Point (10, 10), Point (50, 50)))
