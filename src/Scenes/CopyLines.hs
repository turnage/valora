module Scenes.CopyLines
  ( scene
  ) where

import Control.Monad.Reader
import Control.Monad.State
import Data.Maybe
import Data.RVar
import Data.Random.Distribution.Normal
import Data.Random.Distribution.Uniform
import qualified Data.Vector as V
import Linear

import Continuous
import Coords
import Core
import Geom
import Patterns.Stripes
import Stroke
import Traits.Meta
import Traits.Transform
import Warp
import Wiggle

scene :: Generate ()
scene = do
  World {height, ..} <- asks world
  let sampler = RandomSampler $ normal 0 1
  sqSize <- sampleRVar $ uniform 1 10
  let dot = square (V2 0 0) sqSize
  hsva 100 0.2 1 1
  maybe (pure ()) (id) $ drawContour $ square (V2 0 0) (fromIntegral height)
  strokes' <- strokes
  hsva 260 1 1 1
  strokeRasters <-
    sequence $
    map (\stroke -> runRand $ rasterStroke stroke 1000 sampler dot) strokes'
  let dots = concat strokeRasters
  foldr1 (>>) $ mapMaybe (drawContour) dots

strokes :: Generate [Stroke]
strokes = do
  stripeCount :: Double <- sampleRVar $ normal 20 5
  lines <- stripes StripesCfg {stripeCount = round stripeCount}
  lines' <- runRand $ sequence $ map (warp warpCfgDefault) lines
  return $ map (mkStroke . vertices) lines'
