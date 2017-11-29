module Coords.Math
  ( distance
  , leastDistance
  , circumPoint
  , midpoint
  ) where

import qualified Data.Vector as V

import Coords (Point(..))

distance :: Point -> Point -> Double
distance p1 p2 = sqrt $ x ^ 2 + y ^ 2
  where
    Point {x, y} = abs $ p1 - p2

midpoint :: Point -> Point -> Point
midpoint (Point {x = x1, y = y1}) (Point {x = x2, y = y2}) =
  Point {x = x1 + (x2 - x1) / 2, y = y1 + (y2 - y1) / 2}

circumPoint :: Point -> Double -> Double -> Point
circumPoint Point {x = xStart, y = yStart} radius angle = Point {x = x', y = y'}
  where
    x' = xStart + radius * (cos angle)
    y' = yStart + radius * (sin angle)

leastDistance :: V.Vector Point -> Point -> Double
leastDistance points anchor = V.minimum $ V.map (distance anchor) points
