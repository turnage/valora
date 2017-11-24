module Poly.Properties
  ( Edge(..)
  , centroid
  , connect
  , extent
  , inExtent
  , vertexPairs
  , edges
  ) where

import qualified Data.Vector as V

import Poly

data Edge = Edge
  { start :: Point
  , end :: Point
  } deriving (Eq, Show)

centroid :: Poly -> Point
centroid Poly {vertices} =
  Point {x = (right + left) / 2, y = (top + bottom) / 2}
  where
    left = V.minimum xs
    right = V.maximum xs
    top = V.maximum ys
    bottom = V.minimum ys
    ys = V.map (y) vertices
    xs = V.map (x) vertices

connect :: Point -> Point -> Edge
connect p1 p2 = Edge {start = p1, end = p2}

data Extent = Extent
  { bottomRight :: Point
  , size :: Double
  } deriving (Eq, Show)

-- Extent returns a square that encloses all vertices of the poly.
extent :: Poly -> Extent
extent Poly {vertices} = Extent {bottomRight, size}
  where
    bottomRight = Point {x = lowX, y = lowY}
    size = maximum [highY - lowY, highX - lowX]
    highY = y $ V.maximumBy (compareBy (y)) vertices
    lowY = y $ V.minimumBy (compareBy (y)) vertices
    highX = x $ V.maximumBy (compareBy (x)) vertices
    lowX = x $ V.minimumBy (compareBy (x)) vertices
    compareBy f v1 v2 = compare (f v1) (f v2)

inExtent :: Extent -> Point -> Bool
inExtent Extent {bottomRight = Point {x = lowX, y = lowY}, size} Point {x, y} =
  x >= lowX && x < lowX + size && y >= lowY && y < lowY + size

vertexPairs :: Poly -> V.Vector (Point, Point)
vertexPairs Poly {vertices} = V.zip vertices rotatedVertices
  where
    rotatedVertices = V.backpermute vertices rotationIndices
    rotationIndices = V.generate vLen (\i -> (i + 1) `mod` vLen)
    vLen = V.length vertices

edges :: Poly -> V.Vector Edge
edges poly = V.map (\(a, b) -> Edge {start = a, end = b}) $ vertexPairs poly
