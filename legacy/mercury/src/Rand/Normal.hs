module Rand.Normal
  ( Normal(..)
  ) where

import qualified Data.Random.Normal as N
import System.Random

newtype Normal =
  Normal StdGen

instance RandomGen Normal where
  next (Normal rng) = (v', Normal rng')
    where
      v' = floor $ (fromIntegral (maxBound :: Int)) * v
      (v :: Double, rng') = N.normal rng
  split (Normal rng) = (Normal rng', Normal rng'')
    where
      (rng', rng'') = split rng
  genRange _ = (minBound, maxBound)
