module Traits.Transform
  ( Subdivide(..)
  ) where

class Subdivide s where
  subdivide :: s -> s
  subdivision :: s -> Int -> s
  subdivision s n = (iterate (subdivide) s) !! n
