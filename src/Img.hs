module Img
  ( Raster
  , Layer
  , dims
  , fromMap
  , newLayer
  , fillLayer
  , fillRow
  , applyLayer
  , rasterLayer
  , RGBA
  ) where

import Data.Array.Repa (Array, DIM2, DIM3, Z(..), (:.)(..))
import qualified Data.Array.Repa as R
import qualified Data.HashMap as H
import Data.Word (Word8)

import Shape

type Raster = Array R.U DIM2 Dot

type Dot = (Word8, Word8, Word8)

type RGBA = (Double, Double, Double, Double)

type Layer = Array R.D DIM2 RGBA

dims :: Layer -> (Int, Int)
dims layer =
  let (Z :. h :. w) = R.extent layer
  in (w, h)

fromMap :: (Int, Int) -> H.Map (Int, Int) RGBA -> Layer
fromMap (w, h) map = R.traverse (newLayer (w, h)) id maybeColor
  where
    maybeColor indx (Z :. y :. x) = H.findWithDefault (0, 0, 0, 0) (x, y) map

newLayer :: (Int, Int) -> Layer
newLayer (w, h) = R.traverse (raw (h, w)) packDims packPixel
  where
    packDims (Z :. h :. w :. c) = (Z :. h :. w)
    packPixel indx (Z :. y :. x) =
      ( indx (Z :. y :. x :. 0)
      , indx (Z :. y :. x :. 1)
      , indx (Z :. y :. x :. 2)
      , indx (Z :. y :. x :. 3))

fillRow :: RGBA -> (Int, Int, Int) -> Layer -> Layer
fillRow color (a, b1, b2) layer = R.traverse layer id colorIf
  where
    colorIf indx (Z :. sa :. sb) =
      if sa == a && sb >= b1 && sb <= b2
        then color
        else indx (Z :. sa :. sb)

fillLayer :: RGBA -> Layer -> Layer
fillLayer color layer = R.map (\_ -> applyPixel (0, 0, 0, 0) color) layer

applyLayer :: Layer -> Layer -> Layer
applyLayer bottom top = R.traverse bottom id applyPixelProxy
  where
    applyPixelProxy indx pos = applyPixel (indx pos) (top R.! pos)

rasterLayer :: Layer -> Raster
rasterLayer layer =
  let [img] = R.computeP $ R.map rasterPixel layer
  in img

raw :: (Int, Int) -> Array R.U DIM3 Double
raw (w, h) = R.fromListUnboxed (Z :. w :. h :. 4) (take (w * h * 4) (cycle [0]))

applyPixel (br, bg, bb, ba) (tr, tg, tb, ta) = (fc br tr, fc bg tg, fc bb tb, denom)
  where
    denom = ta + tb * (1 - ta)
    fc b t = ta * t + ba * b * (1 - ta) / (ta + tb * (1 - ta))

rasterPixel (r, g, b, _) = (f r, f g, f b)
  where
    f = floor . (* 255)
