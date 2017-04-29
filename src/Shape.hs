module Shape
  ( Polygon(..)
  , Rect(..)
  , Edge
  ) where

import qualified Data.Vector as V

-- Point (x, y)
newtype Point =
  Point (Int, Int)

newtype Edge =
  Edge (Point, Point)

data ScanEdge = ScanEdge
  { low :: Int
  , high :: Int
  , x :: Double
  , deltaX :: Double
  , deltaY :: Double
  }

scanEdge :: Edge -> ScanEdge
scanEdge (Edge ((Point (x1, y1)), (Point (x2, y2)))) = ScanEdge
  { low = min y1 y2
  , high = max y1 y2
  , x = fromIntegral $ if min y1 y2 == y1 then x1 else x2
  , deltaX = fromIntegral $ x2 - x1
  , deltaY = fromIntegral $ y2 - y1
  }

scanEdges :: Polygon p => p -> V.Vector ScanEdge
scanEdges pol = V.map (scanEdge) (edges pol)

class Polygon p where
  vertices :: p -> [Point]

edges :: Polygon p => p -> V.Vector Edge
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
