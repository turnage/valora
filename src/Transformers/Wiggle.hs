module Transformers.Wiggle
  ( wiggle
  ) where

import Coords (Point(..))
import Rand
import Transformers

wiggle :: Double -> Transformer Point Point
wiggle strength =
  Transformer {consumedSamples = 2, runTransform = wiggleImpl strength}

wiggleImpl :: Double -> SampleFeed -> Point -> Point
wiggleImpl strength (SampleFeed (Sample s1:Sample s2:_)) (Point {x, y}) =
  Point {x = x', y = y'}
  where
    x' = x + strength * s1
    y' = y + strength * s2
