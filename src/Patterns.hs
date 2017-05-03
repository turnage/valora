module Patterns
  ( tile
  ) where

import qualified Data.HashMap as H

import Shape

tile :: Int -> Int -> [Rect]
tile frame tiles = rects
  where
    rects =
      map
        (\(x, y) ->
           Rect
           { topLeft = Point {x = x, y = y}
           , bottomRight = Point {x = x + tileSize, y = y + tileSize}
           })
        $ concat corners
    corners = map (\b -> zip borders (repeat b)) borders
    borders = take tiles $ 0 : zipWith (+) borders (repeat tileSize)
    tileSize = frame `div` tiles
