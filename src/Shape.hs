module Shape
  ( Polygon(..)
  , Rect(..)
  , Edge(..)
  , Point(..)
  ) where

import qualified Data.Vector as V

-- Point (x, y)
newtype Point =
  Point (Int, Int)
  deriving (Eq, Show)

newtype Edge =
  Edge (Point, Point)
  deriving (Eq, Show)

class Polygon p where
  vertices :: p -> [Point]
  edges :: p -> V.Vector Edge
  edges pol =
    let vs = vertices pol
        edges = [(Edge ((last vs), (head vs)))] ++ (vertexPairs vs)
    in V.fromList edges

-- Rect ((top left), (bottom right))
data Rect =
  Rect (Point, Point)

instance Polygon Rect where
  vertices (Rect ((Point (x1, y1)), (Point (x2, y2)))) =
    [(Point (x1, y1)), (Point (x2, y1)), (Point (x2, y2)), (Point (x1, y2))]

vertexPairs :: [Point] -> [Edge]
vertexPairs vs =
  if length vs < 2
    then []
    else [(Edge (head vs, head (tail vs)))] ++ vertexPairs (tail vs)
