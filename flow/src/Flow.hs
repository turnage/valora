module Flow
  ( FlowCfg(..)
  , flow
  ) where

import Control.Monad.Reader
import Data.Fixed
import Data.RVar

import Coords
import Core
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
        in place (pacmanF' $ pivotF $ slideX point xDelta) p
  let animate speed row = map (\point -> locate speed point) row
  World {width, height, ..} <- asks world
  grid' <- grid (width * 2) (height * 2) rows' cols'
  return $ concat $ map (uncurry animate) $ zip streamSpeeds grid'
