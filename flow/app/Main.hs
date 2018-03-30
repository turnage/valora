module Main where

import Data.Colour.RGBSpace
import Data.Colour.RGBSpace.HSV
import Data.Maybe
import Data.Random.Source.PureMT
import Graphics.Rendering.Cairo

import Coords
import Core
import Geom
import Isotile

width_ = 500

height_ = 500

world :: World
world = World 500 500 1 2

main :: IO ()
main = screen world $ realize world (pureMT 50) scene

scene :: Generate ()
scene =
  cairo $ do
    setSourceRGBA 1 0 0 1
    foldr1 (>>) $
      map (>> fill) $
      mapMaybe (drawContour . (isoSideMask IsoLeft)) $
      isotileGrid 80 width_ height_
    setSourceRGBA 0 0 1 1
    foldr1 (>>) $
      map (>> fill) $
      mapMaybe (drawContour . (isoSideMask IsoCenter)) $
      isotileGrid 80 width_ height_
    setSourceRGBA 0 1 0 1
    foldr1 (>>) $
      map (>> fill) $
      mapMaybe (drawContour . (isoSideMask IsoRight)) $
      isotileGrid 80 width_ height_
