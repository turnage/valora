module Warp
  ( WarpCfg(..)
  , warp
  , warpCfgDefault
  ) where

import Core
import Traits.Transform
import Wiggle

data WarpCfg = WarpCfg
  { warpWiggleCfg :: WiggleCfg
  , warpDepth :: Int
  }

warpCfgDefault = WarpCfg {warpWiggleCfg = wiggleCfgDefault, warpDepth = 3}

warp :: (Wiggle w, Subdivide w) => WarpCfg -> w -> Random w
warp WarpCfg {warpWiggleCfg, warpDepth} w =
  iterate (>>= warpRound warpWiggleCfg) (warpRound warpWiggleCfg w) !!
  (warpDepth - 1)

warpRound :: (Wiggle w, Subdivide w) => WiggleCfg -> w -> Random w
warpRound cfg = wiggle cfg . subdivide
