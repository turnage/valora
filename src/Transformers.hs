module Transformers
  ( Transformer(..)
  , runTransforms
  ) where

import qualified Data.Vector as V

import Rand

data Transformer a b = Transformer
  { consumedSamples :: Int
  , runTransform :: (SampleFeed -> a -> b)
  }

runTransforms :: Transformer a b
              -> SampleFeed
              -> V.Vector a
              -> (SampleFeed, V.Vector b)
runTransforms (Transformer {consumedSamples, runTransform}) (SampleFeed feed) as =
  (caboose, bs)
  where
    bs = V.map (uncurry runTransform) $ V.zip feedsToConsume as
    (caboose, feedsToConsume) =
      partitionSample consumedSamples (V.length as) $ SampleFeed feed
