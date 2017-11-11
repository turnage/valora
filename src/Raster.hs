module Raster
  ( Rasterable(..)
  , Raster
  , RasterSize
  , RasterCtx(..)
  , Pixel(..)
  , render
  , fromVectorCtx
  , toVectorCtx
  , toPixel
  , fromRasterCoord
  , toRasterCoord
  , fromPixel
  , mapColor
  ) where

import Color (Dot, RGBA(..), collapseColor, emptyColor)
import Data.Array.Repa (Array, DIM2, DIM3, Z(..), (:.)(..))
import qualified Data.Vector as V
import qualified Data.Array.Repa as R
import Poly (Point(..))

type Raster = V.Vector (V.Vector RGBA)

-- Bitmap is a two dimensional array (stored as unboxed vectors)
-- of 24 bit rgb pixels.
type Bitmap = Array R.U DIM2 Dot

type Layer = Array R.D DIM2 RGBA

type RasterSize = Int

data Pixel = Pixel
  { x :: Int
  , y :: Int
  } deriving (Eq, Show)

render :: Raster -> Bitmap
render raster = collapse $ mapColor (color) $ newLayer $ V.length raster
  where
    color (Pixel {x, y}) _ = (raster V.! y) V.! x

data RasterCtx = RasterCtx {
  size :: RasterSize,
  pixel :: Pixel
}

fromVectorCtx :: RasterSize -> Point -> RasterCtx
fromVectorCtx size point = RasterCtx { size, pixel = toPixel size point }

toVectorCtx :: RasterCtx -> Point
toVectorCtx RasterCtx {size, pixel} = fromPixel size pixel

class Rasterable r where
  raster :: RasterSize -> r -> Raster

-- The vector dimensions are 0-1 within our square frame.
toPixel :: RasterSize -> Point -> Pixel
toPixel size Point {x, y} = Pixel { x = toRasterCoord size x, y = toRasterCoord size y}


fromPixel :: RasterSize -> Pixel -> Point
fromPixel size Pixel {x, y} = Point {x = fromRasterCoord size x, y = fromRasterCoord size y }

toRasterCoord :: RasterSize -> Double -> Int
toRasterCoord size coord = floor $ fromIntegral size * coord

fromRasterCoord :: RasterSize -> Int -> Double
fromRasterCoord size coord = (fromIntegral size) * (fromIntegral coord)

collapse :: Layer -> Bitmap
collapse layer =
  let [img] = R.computeP $ R.map collapseColor layer
  in img

mapColor :: (Pixel -> RGBA -> RGBA) -> Layer -> Layer
mapColor f layer = R.traverse layer (id) proxy
  where
    proxy indx (Z :. y :. x) =
      f Pixel {x, y} $ indx (Z :. y :. x)

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
