module Patterns.Sparkles
  ( sparkles
  ) where

import Control.Monad.Random
import qualified Data.Vector as V
import System.Random

import Coords (Point(..))

sparkles
  :: RandomGen g
  => Int -> Rand g (V.Vector Point)
sparkles 0 = return V.empty
sparkles count = V.sequence sparkleGenerators
  where
    sparkleGenerators = V.generate count $ const sparkleGenerator
    sparkleGenerator = do
      x <- getRandomR (0, 1)
      y <- getRandomR (0, 1)
      return Point {x, y}
