module Stroke
  ( Stroke
  , StrokeCfg(..)
  , mkStroke
  , rasterStroke
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

import Continuous
import Core
import Geom
import Traits.Position

data StrokeCfg =
  StrokeCfg

data Stroke = Stroke
  { xSpline :: BezierCurve Double
  , ySpline :: BezierCurve Double
  }

instance Continuous Stroke (V2 Double) where
  sample (Stroke xSpline ySpline) completion =
    if completion >= 0 && completion <= 1
      then Just $ V2 x y
      else Nothing
    where
      x = evalSpline xSpline completion
      y = evalSpline ySpline completion

mkStroke :: V.Vector (V2 Double) -> Stroke
mkStroke vertices = Stroke xSpline ySpline
  where
    xSpline = bezierCurve $ V.map (^. _x) vertices
    ySpline = bezierCurve $ V.map (^. _y) vertices

rasterStroke :: Stroke -> Int -> Sampler -> Contour -> Random [Contour]
rasterStroke stroke n sampler dot = do
  dotPositions <- sampleN stroke n sampler
  return $ map (place dot) dotPositions
