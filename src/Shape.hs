module Shape
  ( Polygon(..)
  , Rect(..)
  , Irregular(..)
  , Edge(..)
  , Point(..)
  ) where

import qualified Data.Vector as V

import Rand

-- Point (x, y)
newtype Point =
  Point (Int, Int)
  deriving (Eq, Show)

newtype Edge =
  Edge (Point, Point)
  deriving (Eq, Show)

class Polygon p where
  edges :: p -> [Edge]
  vertices :: p -> [Point]
  vertices pol = map (\(Edge (point, _)) -> point) $ edges pol


data Irregular =
  Irregular [Edge]

instance Polygon Irregular where
  edges (Irregular edges) = edges

-- Rect ((top left), (bottom right))
data Rect =
  Rect (Point, Point)

instance Polygon Rect where
  edges (Rect ((Point (x1, y1)), (Point (x2, y2)))) =
    let topLeft = Point (x1, y1)
        topRight = Point (x2, y1)
        bottomLeft = Point (x1, y2)
        bottomRight = Point (x2, y2)
    in [ Edge (topLeft, topRight)
       , Edge (topRight, bottomRight)
       , Edge (bottomRight, bottomLeft)
       , Edge (bottomLeft, topLeft)
       ]
