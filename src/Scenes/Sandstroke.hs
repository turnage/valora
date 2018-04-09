module Scenes.Sandstroke
  (
  ) where

import Control.Lens ((^.))
import Control.Monad.Reader
import Data.Maybe
import Data.RVar
import Data.Random.Distribution.Normal
import qualified Data.Vector as V
import Graphics.Rendering.Cairo (fill, rectangle, setSourceRGBA)
import Linear
import Math.Spline

import Core
import Geom
import Traits.Position
