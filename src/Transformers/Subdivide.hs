module Transformers.Subdivide
  ( subdivideEdgesBy
  ) where

import qualified Data.Vector as V

import Coords (Point(..), distance)
import Poly
import Poly.Math
import Poly.Properties
import Rand (SampleFeed(..))
import Transformers

subdivideEdgesBy :: Transformer (Double, Point) Point
                 -> SampleFeed
                 -> Poly
                 -> (SampleFeed, Poly)
subdivideEdgesBy transformer feed Poly {vertices} =
  (feed', Poly {vertices = vertices'})
  where
    vertices' = V.backpermute ((V.++) vertices midpoints') placementIndices
    placementIndices = V.generate (2 * (V.length vertices)) (place)
    place i =
      if odd i
        then (i `div` 2) + V.length vertices
        else i `div` 2
    (feed', midpoints') =
      runTransforms transformer feed $ V.zip distances midpoints
    distances = V.map (uncurry distance) pairs
    midpoints = V.map (uncurry midpoint) pairs
    pairs = vertexPairs Poly {vertices}
