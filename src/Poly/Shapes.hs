module Poly.Shapes
  ( irregular
  , ngon
  ) where

import qualified Data.Vector as V

import Coords (Point(..))
import Coords.Math (circumPoint)
import Poly (Poly(..))

ngon :: Double -> Int -> Point -> Poly
ngon radius n centroid =
  Poly
  { vertices =
      V.generate
        n
        ((circumPoint centroid radius) .
         (* ((2 * pi) / fromIntegral n)) . fromIntegral)
  }

-- I need to pick a line out from centroid and iterate the angle at which I draw a vertex
-- a constant distance from the centroid.
-- The angle for an equilateral ngon is is 360 / n
-- I need a function to permute angle -> Point -> Double -> Point
irregular :: V.Vector Point -> Poly
irregular vertices = Poly {vertices}
