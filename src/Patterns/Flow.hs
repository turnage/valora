module Patterns.Flow
  ( FlowCfg(..)
  , flow
  ) where

import Control.Monad.Reader
import Data.Fixed
import Data.List.Split
import Data.RVar

import Coords
import Core
import Patterns.Grid
import Traits.Position

data FlowCfg = FlowCfg
  { rows :: RVar Double
  , cols :: RVar Double
  , streamSpeed :: RVar Double
  , angle :: RVar Double
  }

flow :: Place p => FlowCfg -> p -> Generate [(Int -> p)]
flow (FlowCfg rows cols streamSpeed angle) p = do
  rows' <- sampleRVar rows >>= \v -> return $ round $ abs v
  cols' <- sampleRVar cols >>= \v -> return $ round $ abs v
  angle' <- sampleRVar angle >>= \v -> return $ v `mod'` (2 * pi)
  streamSpeeds <- sequence $ take rows' $ repeat $ sampleRVar streamSpeed
  center' <- center
  pacmanF' <- pacmanF
  let pivotF = pivot center' angle'
  let locate speed point i =
        let xDelta = fromIntegral i * speed
        in place p $ pacmanF' $ pivotF $ slideX point xDelta
  let animate speed row = map (\point -> locate speed point) row
  World {width, height, ..} <- asks world
  grid' <-
    grid
      gridCfgDefault
      { width = Just (fromIntegral width * 2)
      , height = Just (fromIntegral height * 2)
      , rows = rows'
      , cols = cols'
      }
  return $
    concat $ map (uncurry animate) $ zip streamSpeeds $ chunksOf cols' grid'
