module Constants
  ( rasterSize
  , pixelCoords
  , pixelSize
  ) where

import qualified Data.Vector as V

import Poly (Point(..))

rasterSize :: Int
rasterSize = 2000

pixelSize :: Double
pixelSize = 1.0 / fromIntegral rasterSize

pixelCoords :: V.Vector Point
pixelCoords = realCoords 1

realCoords :: Int -> V.Vector Point
realCoords order = V.generate numBuckets iToReal
  where
    numBuckets = (rasterSize * order) ^ 2

iToReal :: Int -> Point
iToReal i = Point {x, y}
  where
    x = fromRasterCoord pixelX
    y = fromRasterCoord pixelY
    pixelX = i `div` rasterSize
    pixelY = i - (pixelX * rasterSize)
    fromRasterCoord coord = (fromIntegral coord) / (fromIntegral rasterSize)
