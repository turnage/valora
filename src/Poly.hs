module Poly
  ( Poly(..)
  ) where

import qualified Data.Vector as V

import Coords (Point(..))

-- Poly is made of an ordered list of vertices. Edges are implicit between
-- adjacent vertices and wraparound indices.
data Poly = Poly
  { vertices :: V.Vector Point
  } deriving (Eq, Show)
