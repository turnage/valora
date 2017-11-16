module Poly
  ( Poly(..)
  , Point(..)
  , Edge(..)
  , connect
  , square
  , irregular
  ) where

import qualified Data.Vector as V

data Point = Point
  { x :: Double
  , y :: Double
  } deriving (Eq, Show)

instance Num Point where
  (+) (Point {x = x1, y = y1}) (Point {x = x2, y = y2}) =
    Point {x = x1 + x2, y = y1 + y2}
  (*) (Point {x = x1, y = y1}) (Point {x = x2, y = y2}) =
    Point {x = x1 * x2, y = y1 * y2}
  abs (Point {x, y}) = Point {x = abs x, y = abs y}
  negate (Point {x, y}) = Point {x = negate x, y = negate y}
  signum (Point {x, y}) = Point {x = signum x, y = signum y}
  fromInteger i = Point {x = fromInteger i, y = fromInteger i}

data Edge = Edge
  { start :: Point
  , end :: Point
  } deriving (Eq, Show)

connect :: Point -> Point -> Edge
connect p1 p2 = Edge {start = p1, end = p2}

data Poly = Poly
  { edges :: V.Vector Edge
  , centroid :: Point
  }

vertices :: Poly -> V.Vector Point
vertices = (V.map (start)) . edges

centroidOf :: V.Vector Point -> Point
centroidOf points = Point {x = (right + left) / 2, y = (top + bottom) / 2}
  where
    left = V.minimum xs
    right = V.maximum xs
    top = V.maximum ys
    bottom = V.minimum ys
    ys = V.map (y) points
    xs = V.map (x) points

square :: Point -> Double -> Poly
square bottomLeft size =
  Poly {edges = _edges, centroid = centroidOf $ V.map (start) _edges}
  where
    _edges =
      V.fromList
        [ connect topLeft topRight
        , connect topLeft bottomLeft
        , connect topRight bottomRight
        , connect bottomRight bottomLeft
        ]
    topRight = topLeft + width
    topLeft = bottomLeft + height
    bottomRight = bottomLeft + width
    width = Point {x = size, y = 0}
    height = Point {x = 0, y = size}

irregular :: V.Vector Edge -> Poly
irregular edges = Poly {edges, centroid = centroidOf $ V.map (start) edges}
