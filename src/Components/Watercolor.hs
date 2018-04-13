module Components.Watercolor
  ( WatercolorCfg(..)
  , watercolorCfgDefault
  , watercolor
  ) where

import Core
import Geom
import Warp

data WatercolorCfg = WatercolorCfg
  { watercolorAnchor :: Bool
  , watercolorLayers :: Int
  , watercolorWarpCfg :: Int -> WarpCfg
  }

watercolorCfgDefault =
  WatercolorCfg
  { watercolorAnchor = True
  , watercolorLayers = 30
  , watercolorWarpCfg = const $ warpCfgDefault
  }

watercolor :: WatercolorCfg -> Contour -> Random [Contour]
watercolor WatercolorCfg {watercolorAnchor, watercolorLayers, watercolorWarpCfg} c = do
  prewarped <- warp (watercolorWarpCfg 0) c
  let base i =
        if watercolorAnchor
          then prewarped
          else c
  sequence $
    map (\i -> warp (watercolorWarpCfg i) (base i)) [0 .. watercolorLayers]
