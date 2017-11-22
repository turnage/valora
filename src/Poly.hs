module Poly
  ( Poly(..)
  , Point(..)
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

-- Poly is made of an ordered list of vertices. Edges are implicit between
-- adjacent vertices and wraparound indices.
data Poly = Poly
  { vertices :: V.Vector Point
  } deriving (Eq, Show)
