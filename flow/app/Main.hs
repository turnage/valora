module Main where

import Control.Monad.Reader
import Data.Colour.RGBSpace
import Data.Colour.RGBSpace.HSV
import Data.Maybe
import Data.Random.Source.PureMT
import Graphics.Rendering.Cairo

import Coords
import Core
import Geom
import Isotile

world_ :: World
world_ = World 500 500 1 1

main :: IO ()
main = screen world_ scene

scene :: Generate ()
scene = do
  cairo $ setSourceRGBA 1 0 0 1
  grid <- isotileGrid 80
  Context _ frame <- ask
  cairo $
    foldr1 (>>) $
    map (>> fill) $ mapMaybe (drawContour . (isoSideMask IsoLeft)) grid
  cairo $ setSourceRGBA 0 0 (fromIntegral (frame `mod` 100) / 100.0) 1
  cairo $
    foldr1 (>>) $
    map (>> fill) $ mapMaybe (drawContour . (isoSideMask IsoCenter)) grid
  cairo $ setSourceRGBA 0 1 0 1
  cairo $
    foldr1 (>>) $
    map (>> fill) $ mapMaybe (drawContour . (isoSideMask IsoRight)) grid
