module Coords
  ( Point(..)
  , midpoint
  ) where

data Point = Point
  { x :: !Double
  , y :: !Double
  } deriving (Eq, Show)

instance Num Point where
  (+) (Point x1 y1) (Point x2 y2) = Point (x1 + x2) (y1 + y2)
  (*) (Point x1 y1) (Point x2 y2) = Point (x1 * x2) (y1 * y2)
  abs (Point x y) = Point (abs x) (abs y)
  negate (Point x y) = Point (negate x) (negate y)
  signum (Point x y) = Point (signum x) (signum y)
  fromInteger i = Point (fromInteger i) (fromInteger i)

midpoint :: Point -> Point -> Point
midpoint (Point x1 y1) (Point x2 y2) =
  Point ((x2 - x1) / 2 + x1) ((y2 - y1) / 2 + y1)
