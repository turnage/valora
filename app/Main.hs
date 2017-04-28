module Main where

import Data.Array.Repa (Array, DIM2, DIM3, Z(..), (:.)(..))
import qualified Data.Array.Repa as Repa
import Data.Array.Repa.IO.BMP (writeImageToBMP)
import Data.Word (Word8)

import Img

-- Unboxed concrete values indexed by (x, y, channel)
type Image = Array Repa.U DIM2 (Word8, Word8, Word8)

type ImagePixel = (Word8, Word8, Word8)

blend :: RGBA -> RGBA -> RGBA
blend (br, bg, bb, ba) (tr, tg, tb, ta) = (fc br tr, fc bg tg, fc bb tb, denom)
  where
    denom = ta + tb * (1 - ta)
    fc b t = ta * t + ba * b * (1 - ta) / (ta + tb * (1 - ta))

mapRGB :: (Double -> Double) -> RGBA -> RGBA
mapRGB f (r, g, b, a) = (f r, f g, f b, a)

black :: RGBA
black = (0, 0, 0, 1)

toImagePixel :: RGBA -> RGBA -> ImagePixel
toImagePixel bg p =
  let (r, g, b, _) = mapRGB (* 255) (blend bg p)
  in (floor r, floor g, floor b)

render :: RGBA -> Layer -> Image
render bg r = Repa.computeS $ Repa.map (toImagePixel bg) r

main :: IO ()
main = writeImageToBMP "out.bmp" $ render black $ newLayer (500, 500)
