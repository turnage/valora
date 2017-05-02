module WaterColor
  ( waterColor
  ) where

import qualified Data.HashMap as H
import Data.Traversable (mapAccumR)

import Draw
import Img
import Rand
import Shape
import Warp

waterColor
  :: Polygon p
  => [(RGBA, p)] -> (RGBA -> RGBA -> RGBA) -> H.Map (Int, Int) RGBA
waterColor polies mixer = H.unionsWith mixer bitmaps
  where
    bitmaps = map (\(i, (color, poly)) -> draw (shade i color) poly) $ zip [1 ..] polies
    shade i (r, g, b, a) = (r, g, b, a * (opacity i))
    opacity i = (m i) ^ 6
    m i = (fromIntegral i) / (fromIntegral layers)
    layers = length polies
