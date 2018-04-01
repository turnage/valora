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
  rows' <- sampleRVar rows >>= \v -> return $ round v
  cols' <- sampleRVar cols >>= \v -> return $ round v
  angle' <- sampleRVar angle >>= \v -> return $ v `mod'` (2 * pi)
  streamSpeeds <- sequence $ take rows' $ repeat $ sampleRVar streamSpeed
  Context (World width height _ _) _ <- ask
  let locate speed (Point x y) i =
        let x' = (x + xDelta) `mod'` fromIntegral width
            y' = (y + yDelta) `mod'` fromIntegral height
            xDelta = fromIntegral i * cos angle' * speed
            yDelta = fromIntegral i * sin angle' * speed
        in place (Point x' y') p
  let animate speed row = map (\point -> locate speed point) row
  grid' <- grid rows' cols'
  return $ concat $ map (uncurry animate) $ zip streamSpeeds grid'
