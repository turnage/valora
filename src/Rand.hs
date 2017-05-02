module Rand
  ( Dist(..)
  , Gaussian(..)
  , gaussianBySeed
  ) where

import System.Random

class Dist d where
  randPair :: d -> ((Double, Double), d)

data Gaussian = Gaussian
  { dev :: Double
  , rng :: StdGen
  }

gaussianBySeed :: Int -> Double -> Gaussian
gaussianBySeed seed variance = Gaussian {dev = sqrt variance, rng = mkStdGen seed}

-- Polar Box-Muller transformation.
instance Dist Gaussian where
  randPair (Gaussian {dev = dev, rng = rng}) = fitDist $ until saturates (shift . gen) ((0, 0), rng)
    where
      fitDist ((u, v), rng) = ((x, y), Gaussian {dev = dev, rng = rng})
        where
          s = sOf (u, v)
          m = sqrt (-2 * (log s) / s)
          x = dev * m * u
          y = dev * m * v
      saturates (pair, _) = s /= 0 && s < 1
        where
          s = sOf pair
      sOf (u, v) = u ^ 2 + v ^ 2
      gen (_, rng) = uniformPair rng
      shift ((u, v), rng) = ((u * 2 - 1, v * 2 - 1), rng)

uniformPair :: StdGen -> ((Double, Double), StdGen)
uniformPair rng = ((v1, v2), rng'')
  where
    (v1, rng') = uniform rng
    (v2, rng'') = uniform rng'

uniform :: StdGen -> (Double, StdGen)
uniform rng = randomR (0, 1) rng
