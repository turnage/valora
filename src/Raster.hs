module Raster
  ( Raster
  , RasterSize
  , Pixel(..)
  , Layer(..)
  , raster
  , newLayer
  , mapColor
  , toRasterCoord
  , fromRasterCoord
  ) where

import Color (Dot, RGBA(..), collapseColor, emptyColor)
import Data.Array.Repa (Array, DIM2, DIM3, Z(..), (:.)(..))
import qualified Data.Array.Repa as R
import Poly (Point(..))

-- Raster is a two dimensional array (stored as unboxed vectors)
-- of 24 bit rgb pixels.
type Raster = Array R.U DIM2 Dot

type Layer = Array R.D DIM2 RGBA

type RasterSize = Int

data Pixel = Pixel
  { pixelX :: Int
  , pixelY :: Int
  } deriving (Eq, Show)

-- The vector dimensions are 0-1 within our square frame.
toRasterCoord :: RasterSize -> Double -> Int
toRasterCoord size coord = floor $ fromIntegral size * coord

fromRasterCoord :: RasterSize -> Int -> Double
fromRasterCoord size coord = (fromIntegral size) * (fromIntegral coord)

raster :: Layer -> Raster
raster layer =
  let [img] = R.computeP $ R.map collapseColor layer
  in img

mapColor :: (Pixel -> RGBA -> RGBA) -> Layer -> Layer
mapColor f layer = R.traverse layer (id) proxy
  where
    proxy indx (Z :. pixelY :. pixelX) =
      f Pixel {pixelX, pixelY} $ indx (Z :. pixelY :. pixelX)

newLayer :: RasterSize -> Layer
newLayer size = R.traverse (raw size) packDims packPixel
  where
    packDims (Z :. h :. w :. c) = (Z :. h :. w)
    packPixel indx (Z :. y :. x) =
      RGBA
      { red = indx (Z :. y :. x :. 0)
      , green = indx (Z :. y :. x :. 1)
      , blue = indx (Z :. y :. x :. 2)
      , alpha = indx (Z :. y :. x :. 3)
      }

raw :: RasterSize -> Array R.U DIM3 Double
raw size =
  R.fromListUnboxed
    (Z :. size :. size :. 4)
    (take (size * size * 4) (cycle [0]))
