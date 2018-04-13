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
import Graphics.Rendering.Cairo as Cairo
import Linear

import Components.Watercolor
import Continuous
import Coords
import Core
import Geom
import Patterns.Grid
import Patterns.Sparkles
import Patterns.Stripes
import Stroke
import Traits.Meta
import Traits.Position
import Traits.Transform
import Warp
import Wiggle

scene :: Generate ()
scene = do
  World {height, ..} <- asks world
  let sampler = RandomSampler $ normal 0 1
  sqSize <- sampleRVar $ uniform 1 6
  let dot = square (V2 0 0) sqSize
  hsva 102 0.5885 0.633 1
  maybe (pure ()) (id) $ drawContour $ square (V2 0 0) (fromIntegral height)
  bg
  strokes' <- strokes
  hsva 165 0.4825 0.2485 1
  density :: Double <- sampleRVar $ normal 1000 100
  strokeRasters <-
    sequence $
    map
      (\stroke -> runRand $ rasterStroke stroke (round density) sampler dot)
      strokes'
  let dots = concat strokeRasters
  foldr1 (>>) $ mapMaybe (drawContour) dots
  sparkles <- sequence $ map (const randomPoint) [0 .. 10000]
  let texture = map (place dot) sparkles
  hsva 0 0 1 0.01
  foldr1 (>>) $ mapMaybe (drawContour) texture

bg :: Generate ()
bg = do
  splotchCount :: Int <- sampleRVar $ uniform 50 300
  locations <- sparkles splotchCount
  V.foldr1 (>>) $ V.map (splotch) locations

splotch :: V2 Double -> Generate ()
splotch point = do
  color <- sampleRVar $ normal 102 18
  hsva color 0.5885 0.633 0.05
  splotchSize <- sampleRVar $ normal 40 10
  splotchSpread :: Double <- sampleRVar $ normal 0 30 >>= return . (+ 1) . abs
  let wiggleVar = normal 0 splotchSpread
  let waterCfg =
        watercolorCfgDefault
        { watercolorWarpCfg =
            const
              warpCfgDefault
              { warpWiggleCfg =
                  wiggleCfgDefault {xDist = wiggleVar, yDist = wiggleVar}
              }
        }
  let base = place (square (V2 0 0) splotchSize) point
  splotches <- runRand $ watercolor waterCfg base
  cairo $ setOperator OperatorOver
  foldr1 (>>) $ mapMaybe (drawContour) splotches

strokes :: Generate [Stroke]
strokes = do
  stripeCount :: Double <- sampleRVar $ normal 20 5
  lines <- stripes StripesCfg {stripeCount = round stripeCount}
  lines' <- runRand $ sequence $ map (warp warpCfgDefault) lines
  return $ map (mkStroke . vertices) lines'
