module Coords
  ( midpoint
  , slideX
  , slideY
  , pacman
  , pacmanF
  , distance
  , manhattanDistance
  , pivot
  , center
  ) where

import Control.Monad.Reader
import Data.Fixed
import Linear

import Core

center :: Generate (V2 Double)
center = do
  World width height _ _ <- asks world
  return $ V2 (fromIntegral width / 2) (fromIntegral height / 2)

midpoint :: V2 Double -> V2 Double -> V2 Double
midpoint (V2 x1 y1) (V2 x2 y2) = V2 ((x2 - x1) / 2 + x1) ((y2 - y1) / 2 + y1)

pacman :: V2 Double -> Generate (V2 Double)
pacman point = do
  f <- pacmanF
  return $ f point

pacmanF :: Generate (V2 Double -> V2 Double)
pacmanF = do
  World width height _ _ <- asks world
  let f (V2 x y) =
        V2 (x `mod'` fromIntegral width) (y `mod'` fromIntegral height)
  return f

pivot :: V2 Double -> Double -> V2 Double -> V2 Double
pivot (V2 x y) theta point = V2 rX rY
  where
    r = distance (V2 x y) point
    V2 xDelta yDelta = point - (V2 x y)
    rX = x + cos theta * xDelta - sin theta * yDelta
    rY = y + sin theta * xDelta + cos theta * yDelta

manhattanDistance :: V2 Double -> V2 Double -> Double
manhattanDistance (V2 x1 y1) (V2 x2 y2) = abs (x2 - x1) + abs (y2 - y1)

slideX :: V2 Double -> Double -> V2 Double
slideX (V2 x y) slide = V2 (x + slide) y

slideY :: V2 Double -> Double -> V2 Double
slideY (V2 x y) slide = V2 x (y + slide)
