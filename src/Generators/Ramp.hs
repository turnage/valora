module Generators.Ramp
  ( RampCfg(..)
  , ramp
  ) where

import Control.Monad.Random
import qualified Data.Vector as V

data RampCfg = RampCfg
  { jitter :: Double -- factor to multiply a random sample against (0 for no jitter).
  , steps :: Int
  , factor :: Double -- factor to multiply result against.
  }

ramp
  :: RandomGen g
  => RampCfg -> (a -> Double -> b) -> a -> Rand g (V.Vector b)
ramp cfg f a = do
  samples <- getRandomRs (-1, 1)
  return $ V.map (f a) $ V.fromList $ rampFactors cfg samples

rampFactors :: RampCfg -> [Double] -> [Double]
rampFactors RampCfg {jitter, steps, factor} samples =
  zipWith (factor') [0 .. steps] samples
  where
    factor' i sample = factor * ((proportion i) + (sample * jitter))
    proportion i = fromIntegral i / fromIntegral steps
