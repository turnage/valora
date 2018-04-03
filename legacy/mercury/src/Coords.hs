module Coords
  ( Pixel(..)
  , Point(..)
  , pixelCoords
  , pixelToI
  , pointToI
  , pointToPixel
  , pixelToPoint
  , iToPixel
  , iToPoint
  , Subrange(..)
  , subrangeIToI
  , iToSubrangeValue
  , extractSubrange
  , toRasterCoord
  , fromRasterCoord
  ) where

import qualified Data.Vector as V

import Constants (rasterSize, pixelSize)

pixelCoords :: V.Vector Point
pixelCoords = V.generate (rasterSize ^ 2) (iToPoint)

data Pixel = Pixel
  { x :: Int
  , y :: Int
  } deriving (Eq, Show)

data Point = Point
  { x :: Double
  , y :: Double
  } deriving (Eq, Show)

instance Num Point where
  (+) (Point {x = x1, y = y1}) (Point {x = x2, y = y2}) =
    Point {x = x1 + x2, y = y1 + y2}
  (*) (Point {x = x1, y = y1}) (Point {x = x2, y = y2}) =
    Point {x = x1 * x2, y = y1 * y2}
  abs (Point {x, y}) = Point {x = abs x, y = abs y}
  negate (Point {x, y}) = Point {x = negate x, y = negate y}
  signum (Point {x, y}) = Point {x = signum x, y = signum y}
  fromInteger i = Point {x = fromInteger i, y = fromInteger i}

pointToI :: Point -> Int
pointToI point = pixelToI $ pointToPixel point

iToPoint :: Int -> Point
iToPoint i = pixelToPoint $ iToPixel i

pointToPixel :: Point -> Pixel
pointToPixel Point {x = pointX, y = pointY} =
  Pixel {x = toRasterCoord pointX, y = toRasterCoord pointY}

pixelToPoint :: Pixel -> Point
pixelToPoint Pixel {x = pixelX, y = pixelY} =
  Point {x = fromRasterCoord pixelX, y = fromRasterCoord pixelY}

pixelToI :: Pixel -> Int
pixelToI Pixel {x, y} = x * rasterSize + y

iToPixel :: Int -> Pixel
iToPixel i = Pixel {x = pixelX, y = pixelY}
  where
    pixelX = i `div` rasterSize
    pixelY = i `mod` rasterSize

fromRasterCoord coord = (fromIntegral coord) / (fromIntegral rasterSize)

toRasterCoord coord = floor $ coord * fromIntegral rasterSize

data Subrange a = Subrange
  { subrange :: V.Vector a
  , bottomLeft :: Pixel
  , height :: Int
  , width :: Int
  }

subrangeIToI :: Subrange a -> Int -> Int
subrangeIToI Subrange {bottomLeft = Pixel {x = startX, y = startY}, height, ..} i =
  pixelToI Pixel {x = startX + subrangeX, y = startY + subrangeY}
  where
    subrangeX = i `div` height
    subrangeY = i `mod` height

iToSubrangeValue :: Int -> Subrange a -> Maybe a
iToSubrangeValue i Subrange { bottomLeft = Pixel {x = startX, y = startY}
                            , height
                            , width
                            , subrange
                            } =
  if inSubrange
    then subrange V.!? ((iX - startX) * height + (iY - startY))
    else Nothing
  where
    inSubrange =
      iX >= startX &&
      iX < startX + width && iY >= startY && iY < startY + height
    Pixel {x = iX, y = iY} = iToPixel i

extractSubrange :: Pixel -> Int -> Int -> (Point -> a) -> Subrange a
extractSubrange bottomLeft width height f =
  subrange {subrange = V.generate (width * height) f'}
  where
    f' = (f . (pixelCoords V.!) . (subrangeIToI subrange))
    subrange = Subrange {height, width, bottomLeft, subrange = V.empty}
