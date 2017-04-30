module Rand
  (
    Dist(..),
    Gaussian(..),
    gaussianBySeed,
  ) where

import System.Random

class Dist d where
  rand :: Double -> Double -> d -> (Double, d)

data Gaussian = Gaussian
  { rng :: StdGen
  }

gaussianBySeed :: Int -> Gaussian
gaussianBySeed seed = Gaussian {rng = mkStdGen seed}

-- Polar Box-Muller transformation.
instance Dist Gaussian where
  rand mean dev (Gaussian {rng = rng}) = fitDist $ until saturates gen ((0, 0), rng)
    where
      fitDist ((u, v), rng) = (x, Gaussian {rng = rng})
        where
          s = sOf (u, v)
          m = sqrt (-2 * (log s) / s)
          x = mean + dev * m * u
      saturates (pair, _) = s /= 0 && s < 1
        where
          s = sOf pair
      sOf (u, v) = u ^ 2 + v ^ 2
      gen (_, rng) = uniformPair rng
      adjust v = v * 2 - 1

uniformPair :: StdGen -> ((Double, Double), StdGen)
uniformPair rng = ((v1, v2), alteredRNG)
  where
    (v1, halfAlteredRNG) = uniform rng
    (v2, alteredRNG) = uniform halfAlteredRNG

uniform :: StdGen -> (Double, StdGen)
uniform rng = randomR (0, 1) rng
