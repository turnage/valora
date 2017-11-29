module Transformers.Subdivide
  ( subdivideEdges
  ) where

import Control.Monad.Random
import qualified Data.Vector as V

import Coords (Point(..))
import Coords.Math (midpoint)
import Poly
import Poly.Properties

subdivideEdges
  :: RandomGen g
  => Poly -> Rand g Poly
subdivideEdges Poly {vertices} = return (Poly {vertices = vertices'})
  where
    vertices' = V.backpermute ((V.++) vertices midpoints) placementIndices
    placementIndices = V.generate (2 * (V.length vertices)) (place)
    place i =
      if odd i
        then (i `div` 2) + V.length vertices
        else i `div` 2
    midpoints = V.map (uncurry midpoint) pairs
    pairs = vertexPairs Poly {vertices}
