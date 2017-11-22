module Poly.Shapes
  ( irregular
  , square
  ) where

import qualified Data.Vector as V

import Poly

square :: Point -> Double -> Poly
square bottomLeft size = Poly {vertices}
  where
    vertices = V.fromList [topLeft, topRight, bottomRight, bottomLeft]
    topRight = topLeft + width
    topLeft = bottomLeft + height
    bottomRight = bottomLeft + width
    width = Point {x = size, y = 0}
    height = Point {x = 0, y = size}

irregular :: V.Vector Point -> Poly
irregular vertices = Poly {vertices}
