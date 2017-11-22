module Poly.Properties
  ( Edge(..)
  , centroid
  , connect
  , vertexPairs
  , edges
  ) where

import qualified Data.Vector as V

import Poly

data Edge = Edge
  { start :: Point
  , end :: Point
  } deriving (Eq, Show)

connect :: Point -> Point -> Edge
connect p1 p2 = Edge {start = p1, end = p2}

vertexPairs :: Poly -> V.Vector (Point, Point)
vertexPairs Poly {vertices} = V.zip vertices rotatedVertices
  where
    rotatedVertices = V.backpermute vertices rotationIndices
    rotationIndices = V.generate vLen (\i -> (i + 1) `mod` vLen)
    vLen = V.length vertices

edges :: Poly -> V.Vector Edge
edges poly = V.map (\(a, b) -> Edge {start = a, end = b}) $ vertexPairs poly

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
