module Generators.WaterColor
  ( WaterColorCfg(..)
  , waterColor
  ) where

import Control.Monad (liftM2)
import Control.Monad.Random
import qualified Data.Vector as V

import Color (RGBA(..))
import Color.Shaders (Shader(..), translucer)
import Coords.Math (leastDistance)
import Generators.Color (opacityRamp)
import Generators.Ramp
import Patterns.Sparkles (sparkles)
import Poly (Poly(..))
import Poly.Properties (Extent(..), extent)
import Poly.Properties (Extent(..), extent)
import Transformers.Warp
import Transformers.Wiggle

data WaterColorCfg = WaterColorCfg
  { spread :: Double
  , layers :: Int
  , depth :: Int
  , maxOpacity :: Double
  , strokes :: Int
  }

waterColor
  :: RandomGen g
  => WaterColorCfg -> Shader -> Poly -> Rand g (V.Vector (Shader, Poly))
waterColor WaterColorCfg {spread, layers, depth, maxOpacity, strokes} shader poly = do
  sparkles' <- sparkles strokes
  polies' <- polies $ (1 -) . (leastDistance sparkles')
  shaders' <- shaders
  polies'' <- V.sequence polies'
  return $ V.zip shaders' polies''
  where
    shaders =
      opacityRamp
        RampCfg {jitter = 0, steps = layers, factor = maxOpacity}
        shader
    polies spatialAdapter =
      ramp
        RampCfg {jitter = 0, steps = layers, factor = spread}
        (polyLayer spatialAdapter)
        poly
    polyLayer spatialAdapter poly factor =
      warp
        WarpCfg {depth, wiggleCfg = wiggleCfgForFactor spatialAdapter factor}
        poly
    wiggleCfgForFactor spatialAdapter _ =
      WiggleCfg
      { adaptToNeighbors = True
      , strength = spread * size
      , coverage = Odd
      , expansion = Outward
      , shareSample = False
      , spatialAdapter = Just spatialAdapter
      }
    size = maximum [height, width]
    Extent {height, width, ..} = extent poly
