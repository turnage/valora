module Wiggle
  ( WiggleCfg(..)
  , wigglePoints
  , wiggleCfgDefault
  , simpleWigglePoints
  ) where

import Control.Monad.Reader
import Data.Maybe
import Data.RVar
import Data.Random.Distribution.Normal
import qualified Data.Vector as V
import Linear

import Core

data WiggleCfg = WiggleCfg
    -- wiggleHint receives the index of a vertex and the offset that was sampled for it.
    -- It should return an adjusted offset. If not provided, the offset will be used as
    -- sampled from the provided random variables.
  { wiggleHint :: Maybe (Int -> V2 Double -> V2 Double)
  , xDist :: RVar Double
  , yDist :: RVar Double
  }

wiggleCfgDefault =
  WiggleCfg {wiggleHint = Nothing, xDist = normal 0 1, yDist = normal 0 1}

simpleWigglePoints ::
     RVar Double -> V.Vector (V2 Double) -> Random (V.Vector (V2 Double))
simpleWigglePoints var =
  wigglePoints WiggleCfg {wiggleHint = Nothing, xDist = var, yDist = var}

wigglePoints ::
     WiggleCfg -> V.Vector (V2 Double) -> Random (V.Vector (V2 Double))
wigglePoints WiggleCfg {wiggleHint, xDist, yDist} vertices = do
  points <-
    V.sequence $
    V.map (uncurry offset) $
    V.zip (V.generate (V.length vertices) (id)) vertices
  return points
  where
    hint = maybe (flip const) (id) wiggleHint
    offset i point = do
      xOff <- sampleRVar xDist
      yOff <- sampleRVar yDist
      let offset = hint i $ V2 xOff yOff
      return (offset + point)
