module Rand
  ( Sample(..)
  , SampleFeed(..)
  , partitionSample
  ) where

import Data.List.Split (splitEvery)
import qualified Data.Vector as V

-- Sample is a number between -1 and 1 sampled from a
-- distribution.
newtype Sample =
  Sample Double

-- SampleFeed is an infinite list of samples from a distribution.
newtype SampleFeed =
  SampleFeed [Sample]

partitionSample :: Int -> Int -> SampleFeed -> (SampleFeed, V.Vector SampleFeed)
partitionSample partitionSize partitionCount (SampleFeed feed) =
  (SampleFeed feed', partitions')
  where
    feed' = drop consumedElements feed
    partitions' = V.fromList $ map (\p -> SampleFeed $ p ++ feed') partitions
    partitions = splitEvery partitionSize $ take consumedElements feed
    consumedElements = partitionSize * partitionCount
