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

warp :: Int -> Double -> SampleFeed -> Poly -> V.Vector Poly
warp depth strength feed poly = V.generate 1 $ const $ snd $ results !! depth
  where
    results = iterate warpRound (feed, poly)
    warpRound = uncurry $ subdivideEdgesBy (wiggle strength)
