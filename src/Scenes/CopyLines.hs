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
import Stroke
import Traits.Transform
import Wiggle

scene :: Generate ()
scene = do
  World {width, height, ..} <- asks world
  let middle = fromIntegral width / 2
  let firstLine =
        Line $ V.fromList [V2 middle 0, V2 middle (fromIntegral height)]
  let Line firstLine' = subdivision firstLine 5
  let wiggleCfg =
        WiggleCfg
        {wiggleHint = Nothing, xDist = uniform 200 20, yDist = uniform 5 1}
  firstLine'' <- runRand $ wiggle wiggleCfg firstLine'
  let sampler = UniformSampler
  let dot = square (V2 0 0) 1
  let stroke = mkStroke firstLine''
  hsva 180 1 1 1
  maybe (pure ()) (id) $ drawContour $ square (V2 0 0) (fromIntegral height)
  hsva 260 1 1 1
  strokeRaster <- runRand $ rasterStroke stroke 1000 sampler dot
  foldr1 (>>) $ mapMaybe (drawContour) strokeRaster
