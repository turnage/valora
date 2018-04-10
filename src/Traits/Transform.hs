module Traits.Transform
  ( Subdivide(..)
  , Wiggle(..)
  ) where

import Data.RVar

import Core
import Traits.Meta
import Wiggle

class Subdivide s where
  subdivide :: s -> s
  subdivision :: Int -> s -> s
  subdivision n s = (iterate (subdivide) s) !! n

class Wiggle w where
  wiggle :: WiggleCfg -> w -> Random w
  simpleWiggle :: RVar Double -> w -> Random w

instance Geom2D g => Wiggle g where
  wiggle cfg g = do
    vertices' <- wigglePoints cfg (vertices g)
    return $ fromVertices vertices'
  simpleWiggle r g = do
    vertices' <- simpleWigglePoints r (vertices g)
    return $ fromVertices vertices'
