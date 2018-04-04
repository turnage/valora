module Stroke
  ( Stroke
  , StrokeCfg(..)
  , mkStroke
  , sampleStroke
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

data StrokeCfg =
  StrokeCfg

data Stroke = Stroke
  { xSpline :: BezierCurve Double
  , ySpline :: BezierCurve Double
  }

sampleStroke :: Stroke -> Double -> V2 Double
sampleStroke (Stroke xSpline ySpline) completion = V2 x y
  where
    x = evalSpline xSpline completion
    y = evalSpline ySpline completion

mkStroke :: V.Vector (V2 Double) -> Stroke
mkStroke vertices = Stroke xSpline ySpline
  where
    xSpline = bezierCurve $ V.map (^. _x) vertices
    ySpline = bezierCurve $ V.map (^. _y) vertices
