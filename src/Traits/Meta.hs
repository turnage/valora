module Traits.Meta
  ( Geom2D(..)
  ) where

import qualified Data.Vector as V
import Linear

class Geom2D g where
  vertices :: g -> V.Vector (V2 Double)
  fromVertices :: V.Vector (V2 Double) -> g
