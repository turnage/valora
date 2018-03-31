module Coords
  ( Point(..)
  , midpoint
  , grid
  , slideX
  , slideY
  ) where

import Control.Monad.Reader

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

midpoint :: Point -> Point -> Point
midpoint (Point x1 y1) (Point x2 y2) =
  Point ((x2 - x1) / 2 + x1) ((y2 - y1) / 2 + y1)

slideX :: Point -> Double -> Point
slideX (Point x y) slide = Point (x + slide) y

slideY :: Point -> Double -> Point
slideY (Point x y) slide = Point x (y + slide)

-- row-order grid of points (top left corners)
grid :: Int -> Int -> Generate [[Point]]
grid rows cols = do
  Context (World width height _ _) _ <- ask
  let rowInterval = fromIntegral height / fromIntegral rows
  let colInterval = fromIntegral width / fromIntegral cols
  let point r c =
        Point (fromIntegral c * colInterval) (fromIntegral r * rowInterval)
  return $ map (\r -> map (\c -> point r c) [0 .. cols]) [0 .. rows]
