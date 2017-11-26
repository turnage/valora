module Constants
  ( rasterSize
  , pixelSize
  ) where

import qualified Data.Vector as V

rasterSize :: Int
rasterSize = 1000

pixelSize :: Double
pixelSize = 1.0 / fromIntegral rasterSize
