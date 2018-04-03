module Coords
  ( Point(..)
  , midpoint
  , grid
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

import Core

data Point = Point
  { x :: !Double
  , y :: !Double
  } deriving (Eq, Show)

instance Num Point where
  (+) (Point x1 y1) (Point x2 y2) = Point (x1 + x2) (y1 + y2)
  (*) (Point x1 y1) (Point x2 y2) = Point (x1 * x2) (y1 * y2)
  abs (Point x y) = Point (abs x) (abs y)
  negate (Point x y) = Point (negate x) (negate y)
  signum (Point x y) = Point (signum x) (signum y)
  fromInteger i = Point (fromInteger i) (fromInteger i)

center :: Generate Point
center = do
  World width height _ _ <- asks world
  return $ Point (fromIntegral width / 2) (fromIntegral height / 2)

midpoint :: Point -> Point -> Point
midpoint (Point x1 y1) (Point x2 y2) =
  Point ((x2 - x1) / 2 + x1) ((y2 - y1) / 2 + y1)

pacman :: Point -> Generate Point
pacman point = do
  f <- pacmanF
  return $ f point

pacmanF :: Generate (Point -> Point)
pacmanF = do
  World width height _ _ <- asks world
  let f (Point x y) =
        Point (x `mod'` fromIntegral width) (y `mod'` fromIntegral height)
  return f

pivot :: Point -> Double -> Point -> Point
pivot pivot theta point = Point rX rY
  where
    r = distance pivot point
    Point xDelta yDelta = point - pivot
    rX = (x pivot) + cos theta * xDelta - sin theta * yDelta
    rY = (y pivot) + sin theta * xDelta + cos theta * yDelta

distance :: Point -> Point -> Double
distance (Point x1 y1) (Point x2 y2) = sqrt $ ((x2 - x1) ^ 2) + ((y2 - y1) ^ 2)

manhattanDistance :: Point -> Point -> Double
manhattanDistance (Point x1 y1) (Point x2 y2) = abs (x2 - x1) + abs (y2 - y1)

slideX :: Point -> Double -> Point
slideX (Point x y) slide = Point (x + slide) y

slideY :: Point -> Double -> Point
slideY (Point x y) slide = Point x (y + slide)

-- row-order grid of points (top left corners)
grid :: Int -> Int -> Int -> Int -> Generate [[Point]]
grid width height rows cols = do
  let rowInterval = fromIntegral height / fromIntegral rows
  let colInterval = fromIntegral width / fromIntegral cols
  let point r c =
        Point (fromIntegral c * colInterval) (fromIntegral r * rowInterval)
  return $ map (\r -> map (\c -> point r c) [0 .. cols]) [0 .. rows]
