module Shape
  ( Polygon(..)
  , Rect(..)
  ) where

import qualified Data.Vector as V

-- Point (x, y)
newtype Point =
  Point (Int, Int)

newtype Edge =
  Edge (Point, Point)

data EdgeBucket = EdgeBucket
  { edge :: Edge
  , slope :: (Int -> Int)
  }

type Edges = V.Vector Edge

class Polygon p where
  vertices :: p -> [Point]
  edges :: p -> Edges
  edges pol =
    let vs = vertices pol
        edges = [(Edge ((last vs), (head vs)))] ++ (vertexPairs vs)
    in V.fromList edges

vertexPairs :: [Point] -> [Edge]
vertexPairs vs =
  if length vs == 0
    then []
    else [(Edge (head vs, head (tail vs)))] ++ vertexPairs (tail vs)

-- Rect ((top left), (bottom right))
data Rect =
  Rect (Point, Point)

instance Polygon Rect where
  vertices (Rect ((Point (x1, y1)), (Point (x2, y2)))) =
    [(Point (x1, y1)), (Point (x2, y1)), (Point (x2, y2)), (Point (x1, y2))]
