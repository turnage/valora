module Shape
  ( Polygon(..)
  , Rect(..)
  , PointPoly(..)
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
  vertices :: p -> [Point]
  warp
    :: Dist d
    => d -> p -> (PointPoly, d)
  warp dist pol = (PointPoly $ map (\(Edge (point, _)) -> point) $ warpedEdges, dist')
    where
      (_, dist', warpedEdges) =
        until allEdgesWarped warpAnotherEdge ((V.toList $ edges pol), dist, [])
      allEdgesWarped (unwarpedEdges, _, _) = null unwarpedEdges
      warpAnotherEdge (unwarpedEdges, dist, warpedEdges) = (tail unwarpedEdges, dist', warpedEdges')
        where
          warpedEdges' = warpedEdges ++ [edge'1, edge'2]
          ((edge'1, edge'2), dist') = warpEdge dist (head unwarpedEdges)
  edges :: p -> V.Vector Edge
  edges pol = V.fromList edges
    where
      vs = vertices pol
      edges = [(Edge ((last vs), (head vs)))] ++ (vertexPairs vs)

midpoint :: Edge -> Point
midpoint (Edge (Point (x1, y1), Point (x2, y2))) =
  Point (x1 + (x2 - x1) `div` 2, y1 + (y2 - y1) `div` 2)

warpEdge
  :: Dist d
  => d -> Edge -> ((Edge, Edge), d)
warpEdge dist (Edge (p1, p2)) = ((Edge (p1, pm), Edge (pm, p2)), alteredDist)
  where
    (pm, alteredDist) = warpPoint dist $ midpoint (Edge (p1, p2))

warpPoint
  :: Dist d
  => d -> Point -> (Point, d)
warpPoint dist (Point (x, y)) = (Point (x + floor xoffset, y + floor yoffset), alteredDist)
  where
    ((xoffset, yoffset), alteredDist) = randPair dist

data PointPoly =
  PointPoly [Point]

instance Polygon PointPoly where
  vertices (PointPoly points) = points

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
