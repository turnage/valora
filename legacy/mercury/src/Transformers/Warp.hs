module Transformers.Warp
  ( WarpCfg(..)
  , warp
  ) where

import Control.Monad.Random
import qualified Data.Vector as V

import Coords (Point(..))
import Poly (Poly(..))
import Transformers.Subdivide
import Transformers.Wiggle

data WarpCfg = WarpCfg
  { depth :: Int
  , wiggleCfg :: WiggleCfg
  }

warp
  :: RandomGen g
  => WarpCfg -> Poly -> Rand g Poly
warp WarpCfg {depth = 0, ..} poly = return poly
warp WarpCfg {depth, wiggleCfg} poly = round >>= nextRound
  where
    round = (subdivideEdges poly) >>= (wiggle wiggleCfg)
    nextRound = warp WarpCfg {depth = depth - 1, wiggleCfg}
