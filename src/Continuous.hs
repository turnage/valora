module Continuous
  ( Continuous(..)
  , Sampler(..)
  ) where

import Data.RVar

import Core

data Sampler
  = RandomSampler (RVar Double)
  | UniformSampler

class Continuous c a where
  sampleN :: c -> Int -> Sampler -> Random [a]
  sampleN medium n (RandomSampler r) = do
    seq <- sequence $ map (_sample) [0 .. n]
    return seq
    where
      _sample :: b -> Random a
      _sample =
        const $ do
          t <- sampleRVar r
          return $ sample medium t
  sampleN medium n (UniformSampler) = do
    let sampleTimes = map ((/ fromIntegral n) . fromIntegral) [0 .. n]
    return $ map (sample medium) sampleTimes
  sample :: c -> Double -> a
