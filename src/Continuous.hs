module Continuous
  ( Continuous(..)
  , Sampler(..)
  ) where

import Data.Maybe
import Data.RVar

import Core

data Sampler
  = RandomSampler (RVar Double)
  | UniformSampler

class Continuous c a where
  sampleN :: c -> Int -> Sampler -> Random [a]
  sampleN medium n (RandomSampler r) = do
    seq <- sequence $ map (_sample) [0 .. n]
    return $ mapMaybe (id) seq
    where
      _sample :: b -> Random (Maybe a)
      _sample = const $ sampleRVar r >>= return . (sample medium)
  sampleN medium n (UniformSampler) = do
    let sampleTimes = map ((/ fromIntegral n) . fromIntegral) [0 .. n]
    return $ mapMaybe (sample medium) sampleTimes
  sample :: c -> Double -> Maybe a
