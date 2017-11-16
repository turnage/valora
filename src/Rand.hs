module Rand
  ( Dist(..)
  , Gaussian(..)
  , Sample(..)
  , SignedSample(..)
  , SampleFeed(..)
  , gaussianBySeed
  , signSample
  , partitionSample
  ) where

import Data.List.Split (splitEvery)
import Data.Tuple (swap)
import qualified Data.Vector as V
import System.Random

-- Sample is a number between 0-1 sampled from a distribution.
newtype Sample =
  Sample Double

-- SignedSample is a number between -0.5 and 0.5 sampled from a
-- distribution.
newtype SignedSample =
  SignedSample Double

signSample :: Sample -> SignedSample
signSample (Sample s) = SignedSample $ s - 0.5

-- SampleFeed is an infinite list of sample from a distribution.
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

class Dist d where
  randPair :: d -> (d, (Sample, Sample))
  randFeed :: d -> SampleFeed
  randFeed d = feedPipe $ randPair d

feedPipe
  :: Dist d
  => (d, (Sample, Sample)) -> SampleFeed
feedPipe (d, (s1, s2)) = SampleFeed $ s1 : s2 : caboose
  where
    SampleFeed caboose = randFeed d

data Gaussian = Gaussian
  { dev :: Double
  , rng :: StdGen
  }

gaussianBySeed :: Int -> Double -> Gaussian
gaussianBySeed seed variance =
  Gaussian {dev = sqrt variance, rng = mkStdGen seed}

-- Polar Box-Muller transformation.
instance Dist Gaussian where
  randPair (Gaussian {dev = dev, rng = rng}) =
    wrapSample $ fitDist $ until saturates (shift . gen) (rng, (0, 0))
    where
      fitDist (rng, (u, v)) = (Gaussian {dev = dev, rng = rng}, (x, y))
        where
          s = sOf (u, v)
          m = sqrt (-2 * (log s) / s)
          x = dev * m * u
          y = dev * m * v
      saturates (_, pair) = s /= 0 && s < 1
        where
          s = sOf pair
      sOf (u, v) = u ^ 2 + v ^ 2
      gen (rng, _) = uniformPair rng
      shift (rng, (u, v)) = (rng, (u * 2 - 1, v * 2 - 1))

wrapSample
  :: Dist d
  => (d, (Double, Double)) -> (d, (Sample, Sample))
wrapSample (dist, (n1, n2)) = (dist, (Sample n1, Sample n2))

uniformPair :: StdGen -> (StdGen, (Double, Double))
uniformPair rng = (rng'', (v1, v2))
  where
    (rng', v1) = uniform rng
    (rng'', v2) = uniform rng'

uniform :: StdGen -> (StdGen, Double)
uniform rng = swap $ randomR (0, 1) rng
