module Main where

import Data.Array.Repa (Array, DIM2, DIM3, Z(..), (:.)(..))
import qualified Data.Array.Repa as Repa
import Data.Array.Repa.IO.BMP (writeImageToBMP)
import Data.Word (Word8)

import Img
import Shape

main :: IO ()
main = writeImageToBMP "out.bmp" $ rasterLayer $ fillLayer (1, 1, 0, 1) $ newLayer (500, 500)
