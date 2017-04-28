module Img
  ( Pixel(..)
  , Raster
  , RGBA(..)
  ) where

import Data.Array.Repa (Array, DIM2, DIM3, Z(..), (:.)(..))
import qualified Data.Array.Repa as R
import Data.Word (Word8)

type Raster = Array R.U DIM2 Dot

type Dot = (Word8, Word8, Word8)

class Pixel p where
  apply :: p -> p -> p
  raster :: p -> Dot

newtype RGBA =
  RGBA (Double, Double, Double, Double)
  deriving (Eq, Show)

instance Pixel RGBA where
  apply (RGBA (br, bg, bb, ba)) (RGBA (tr, tg, tb, ta)) = RGBA (fc br tr, fc bg tg, fc bb tb, denom)
    where
      denom = ta + tb * (1 - ta)
      fc b t = ta * t + ba * b * (1 - ta) / (ta + tb * (1 - ta))
  raster (RGBA (r, g, b, _)) = (f r, f g, f b)
    where
      f = floor . (* 255)
