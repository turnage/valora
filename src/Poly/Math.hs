module Poly.Math
  ( midpoint
  ) where

import Coords (Point(..))

midpoint :: Point -> Point -> Point
midpoint (Point {x = x1, y = y1}) (Point {x = x2, y = y2}) =
  Point {x = x1 + (x2 - x1) / 2, y = y1 + (y2 - y1) / 2}
