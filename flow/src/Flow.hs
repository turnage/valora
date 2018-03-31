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
  }

flow :: Place p => FlowCfg -> p -> Generate [(Int -> p)]
flow (FlowCfg rows cols streamSpeed) p = do
  rows' <- sampleRVar rows >>= \v -> return $ round v
  cols' <- sampleRVar cols >>= \v -> return $ round v
  streamSpeeds <- sequence $ take rows' $ repeat $ sampleRVar streamSpeed
  Context (World width _ _ _) _ <- ask
  let locate speed (Point x y) i =
        place
          (Point ((x + speed * fromIntegral i) `mod'` (fromIntegral width)) y)
          p
  let animate speed row = map (\point -> locate speed point) row
  grid' <- grid rows' cols'
  return $ concat $ map (uncurry animate) $ zip streamSpeeds grid'
