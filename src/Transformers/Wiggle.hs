module Transformers.Wiggle
  ( wiggle
  ) where

import Poly
import Rand
import Transformers

wiggle :: Double -> Transformer Point Point
wiggle strength =
  Transformer {consumedSamples = 2, runTransform = wiggleImpl strength}

wiggleImpl :: Double -> SampleFeed -> Point -> Point
wiggleImpl strength (SampleFeed (s1:s2:_)) (Point {x, y}) =
  Point {x = x', y = y'}
  where
    x' = x + strength * s1'
    y' = y + strength * s2'
    SignedSample s1' = signSample s1
    SignedSample s2' = signSample s2
