module VectorUtil
  ( enumerate
  , mapAccum
  ) where

import qualified Data.Vector as V

enumerate :: V.Vector a -> V.Vector (Int, a)
enumerate as = V.zip (V.generate (V.length as) (id)) as

-- TODO: Figure out a better way; this is O(n^2)
mapAccum :: (acc -> a -> (acc, b)) -> acc -> V.Vector a -> (acc, V.Vector b)
mapAccum f acc as = V.foldl (applyF) (acc, V.empty) as
  where
    applyF (acc, bs) a = (acc', V.snoc bs b)
      where
        (acc', b) = f acc a
