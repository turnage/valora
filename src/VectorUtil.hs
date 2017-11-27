module VectorUtil
  ( enumerate
  ) where

import qualified Data.Vector as V

enumerate :: V.Vector a -> V.Vector (Int, a)
enumerate as = V.zip (V.generate (V.length as) (id)) as
