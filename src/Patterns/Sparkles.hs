module Patterns.Sparkles
  ( sparkles
  ) where

import qualified Data.Vector as V
import Linear

import Coords
import Core

sparkles :: Int -> Generate (V.Vector (V2 Double))
sparkles n = V.sequence $ V.generate n $ const randomPoint
