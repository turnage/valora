module Patterns.Tiles
  ( tile
  ) where

import qualified Data.Vector as V

import Coords (Point(..))
import Poly (Poly(..))
import Poly.Shapes (ngon)

tile :: Int -> V.Vector Poly
tile tiles =
  V.map (\(x, y) -> ngon size 4 Point {x, y}) $ V.fromList $ concat corners
  where
    corners = map (\b -> zip borders (repeat b)) borders
    borders = take tiles $ 0 : zipWith (+) borders (repeat size)
    size = 1.0 / fromIntegral tiles
