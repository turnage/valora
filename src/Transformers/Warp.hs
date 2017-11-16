module Transformers.Warp
  ( warp
  ) where

import Control.Lens (traverse, both, toListOf)
import Data.Traversable (mapAccumR)
import qualified Data.Vector as V

import Poly
import Rand
import Transformers.Subdivide
import Transformers.Wiggle

warp :: Double -> SampleFeed -> Poly -> V.Vector Poly
warp strength feed poly =
  V.fromList [subdivideEdgesBy (wiggle strength) feed poly]
