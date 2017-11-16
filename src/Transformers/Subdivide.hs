module Transformers.Subdivide
  ( subdivideEdgesBy
  ) where

import qualified Data.Vector as V

import Poly
import Poly.Math
import Rand
import Transformers

subdivideEdgesBy :: Transformer Point Point -> SampleFeed -> Poly -> Poly
subdivideEdgesBy transformer feed poly = poly {edges = edges'}
  where
    edges' = V.map (\(a, b) -> Edge {start = a, end = b}) edgePairs'
    edgePairs' = V.concat [startPairs', endPairs']
    startPairs' = V.zip starts midpoints
    endPairs' = V.zip ends midpoints'
    (_, midpoints') = runTransforms transformer feed midpoints
    midpoints = V.map (uncurry midpoint) $ V.map (points) $ edges poly
    points Edge {start, end} = (start, end)
    starts = V.map (start) $ edges poly
    ends = V.map (end) $ edges poly
