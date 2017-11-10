{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Poly
  ( Poly(..)
  , Point(..)
  , Edge(..)
  , Square(..)
  , connect
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

class Poly p where
  edges :: p -> V.Vector Edge

data Square = Square
  { topLeft :: Point
  , size :: Double
  }

instance Poly Square where
  edges (Square {topLeft, size}) =
    V.fromList
      [ connect topLeft topRight
      , connect topLeft bottomLeft
      , connect topRight bottomRight
      , connect bottomRight bottomLeft
      ]
    where
      topRight = topLeft + width
      bottomLeft = topLeft + height
      bottomRight = topLeft + width + height
      width = Point {x = size, y = 0}
      height = Point {x = 0, y = size}
