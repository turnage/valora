module Rand.Spatial
  ( SpatialDist(..)
  ) where

import Coords (Point)
import Rand (Sample)

class SpatialDist s where
  pointSample :: s -> Point -> Sample
