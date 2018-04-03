module Raster
  ( Raster
  , Pixel(..)
  , render
  , rasterWith
  , mapColor
  , emptyRaster
  ) where

import Data.Array.Repa (Array, DIM2, DIM3, Z(..), (:.)(..))
import qualified Data.Array.Repa as R
import qualified Data.Vector as V
import Data.Word (Word8)

import Color (RGBA(..), collapseColor, emptyColor)
import Color.Shaders (Shader(..))
import Constants (rasterSize)
import Coords (Point(..), Pixel(..), pixelCoords)

type Raster = V.Vector RGBA

emptyRaster :: Raster
emptyRaster = V.generate (rasterSize * rasterSize) $ const emptyColor

-- Bitmap is a two dimensional array (stored as unboxed vectors)
-- of 24 bit rgb pixels.
type Bitmap = Array R.U DIM2 (Word8, Word8, Word8)

type Layer = Array R.D DIM2 RGBA

render :: Raster -> Bitmap
render raster = collapse $ mapColor (color) $ newLayer
  where
    color (Pixel {x, y}) _ = raster V.! (x * rasterSize + y)

rasterWith :: (Point -> RGBA) -> Raster
rasterWith f = V.generate (rasterSize * rasterSize) f'
  where
    f' i = f $ pixelCoords V.! i

collapse :: Layer -> Bitmap
collapse layer =
  let [img] = R.computeP $ R.map collapseColor layer
  in img

mapColor :: (Pixel -> RGBA -> RGBA) -> Layer -> Layer
mapColor f layer = R.traverse layer (id) proxy
  where
    proxy indx (Z :. y :. x) = f Pixel {x, y} $ indx (Z :. y :. x)

newLayer :: Layer
newLayer = R.traverse raw packDims packPixel
  where
    packDims (Z :. h :. w :. c) = (Z :. h :. w)
    packPixel indx (Z :. y :. x) =
      RGBA
      { red = indx (Z :. y :. x :. 0)
      , green = indx (Z :. y :. x :. 1)
      , blue = indx (Z :. y :. x :. 2)
      , alpha = indx (Z :. y :. x :. 3)
      }

raw :: Array R.U DIM3 Double
raw =
  R.fromListUnboxed
    (Z :. rasterSize :. rasterSize :. 4)
    (take (rasterSize * rasterSize * 4) (cycle [0]))
