module Raster.Mask
  ( Mask(..)
  , rasterMasks
  ) where

import Data.Maybe (fromMaybe)
import qualified Data.Vector as V

import Color (Blender, RGBA, emptyColor)
import Constants (rasterSize)
import Coords (Subrange(..), iToSubrangeValue)
import Poly (Poly(..))
import Poly.Properties (extent)
import Raster (Raster(..))

type Mask = Subrange RGBA

rasterMasks :: Blender -> V.Vector Mask -> Raster
rasterMasks blender masks = V.generate (rasterSize ^ 2) foldMasks
  where
    foldMasks i =
      V.foldr (blender) emptyColor $ V.mapMaybe (iToSubrangeValue i) masks
