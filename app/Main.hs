{-# LANGUAGE NamedFieldPuns #-}

module Main where

import Data.Array.Repa.IO.BMP (writeImageToBMP)

import Color
import qualified Data.Vector as V
import Patterns
import Poly
import Rand
import Raster (Rasterable(..), render)
import Scan
import Transformers.Warp

main :: IO ()
main = writeImageToBMP "new.bmp" $ render $ raster 500 outScan
  where
    outScan = scan (standardBlender) $ V.fromList [sqgon]
    sqgon = preprocessPoly tileColor sq'
    tileColor = RGBA {red = 1, green = 0, blue = 0, alpha = 1}
    sq' = V.head $ (warp 1) (randFeed $ gaussianBySeed 10 10) sq
    sq = square Point {x = 0.1, y = 0.1} 0.3
