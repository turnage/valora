module Transformers.Wiggle
  ( wiggle
  , adaptiveWiggle
  ) where

import Coords (Point(..))
import Rand
import Transformers

adaptiveWiggle :: Double -> Transformer (Double, Point) Point
adaptiveWiggle strength =
  Transformer {consumedSamples = 2, runTransform = wiggleImpl strength}

wiggle :: Double -> Transformer (Double, Point) Point
wiggle strength =
  Transformer
  { consumedSamples = 2
  , runTransform = (\feed (_, point) -> wiggleImpl strength feed (1, point))
  }

wiggleImpl :: Double -> SampleFeed -> (Double, Point) -> Point
wiggleImpl strength (SampleFeed (Sample s1:Sample s2:_)) ((localStrength, Point { x
                                                                                , y
                                                                                })) =
  Point {x = x', y = y'}
  where
    x' = x + strength * s1 * localStrength
    y' = y + strength * s2 * localStrength
