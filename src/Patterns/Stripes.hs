module Patterns.Stripes
  ( StripesCfg(..)
  , stripes
  ) where

import Control.Lens ((^.))
import Control.Monad.Reader
import qualified Data.Vector as V
import Linear

import Coords
import Core
import Geom
import Patterns.Grid

data StripesCfg = StripesCfg
  { stripeCount :: Int
  }

stripes :: StripesCfg -> Generate [Line]
stripes StripesCfg {stripeCount} = do
  World {height, ..} <- asks world
  let mkLine p =
        Line $ V.fromList [p, V2 (p ^. _x) (fromIntegral height - p ^. _y)]
  gridPoints <-
    grid gridCfgDefault {rows = 1, cols = stripeCount, tileFocus = TopCenter}
  return $ map (mkLine) gridPoints
