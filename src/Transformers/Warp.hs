module Transformers.Warp
  ( warp
  , adaptiveWarp
  ) where

import Control.Lens (traverse, both, toListOf)
import Data.Traversable (mapAccumR)
import qualified Data.Vector as V

import Coords (Point(..))
import Poly (Poly(..))
import Rand
import Transformers (Transformer(..))
import Transformers.Subdivide
import Transformers.Wiggle (wiggle, adaptiveWiggle)

warp :: Int -> Double -> SampleFeed -> Poly -> Poly
warp depth strength feed poly = warpImpl (wiggle) depth strength feed poly

adaptiveWarp :: Int -> Double -> SampleFeed -> Poly -> Poly
adaptiveWarp depth strength feed poly =
  warpImpl (adaptiveWiggle) depth strength feed poly

warpImpl
  :: (Double -> Transformer (Double, Point) Point)
  -> Int
  -> Double
  -> SampleFeed
  -> Poly
  -> Poly
warpImpl transformerF depth strength feed poly = snd $ results !! depth
  where
    results = iterate warpRound (feed, poly)
    warpRound = uncurry $ subdivideEdgesBy (transformerF strength)
