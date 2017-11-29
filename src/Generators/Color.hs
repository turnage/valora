module Generators.Color
  ( opacityRamp
  ) where

import Control.Monad.Random
import qualified Data.Vector as V

import Color (RGBA(..))
import Color.Shaders (Shader(..), translucer)
import Generators.Ramp

opacityRamp
  :: RandomGen g
  => RampCfg -> Shader -> Rand g (V.Vector Shader)
opacityRamp cfg shader = ramp cfg (translucer) shader
