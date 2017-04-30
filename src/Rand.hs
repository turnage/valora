module Rand
  (
  ) where

import Control.Monad.Loops
import System.Random

class Dist d where
  randPair :: d -> IO (Double, Double)

data Gaussian = Gaussian
  { mean :: Double
  , dev :: Double
  }

-- Polar Box-Muller transformation.
instance Dist Gaussian where
  randPair (Gaussian {mean = mean, dev = dev}) = iterateUntil saturates gen >>= (return . fitDist)
    where
      fitDist (u, v) = (x, y)
        where
          s = sOf (u, v)
          m = sqrt (-2 * (log s) / s)
          x = mean + dev * m * u
          y = mean + dev * m * u
      saturates pair = s /= 0 && s < 1
        where
          s = sOf pair
      sOf (u, v) = u ^ 2 + v ^ 2
      gen = uniformPair >>= \(a, b) -> return (adjust a, adjust b)
      adjust v = v * 2 - 1

uniformPair :: IO (Double, Double)
uniformPair = uniformSingle >>= \a -> uniformSingle >>= \b -> return (a, b)

uniformSingle :: IO Double
uniformSingle = getStdRandom (randomR (0, 1))
