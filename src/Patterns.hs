{-# LANGUAGE NamedFieldPuns #-}

module Patterns
  ( tile
  ) where

import qualified Data.Vector as V
import Poly

tile :: Int -> V.Vector Square
tile tiles =
  V.map (\(x, y) -> Square {topLeft = Point {x, y}, size}) $
  V.fromList $ concat corners
  where
    corners = map (\b -> zip borders (repeat b)) borders
    borders = take tiles $ 0 : zipWith (+) borders (repeat size)
    size = 1.0 / fromIntegral tiles
