module Main where

import Data.Array.Repa (Array, DIM2, DIM3, Z(..), (:.)(..))
import qualified Data.Array.Repa as Repa
import Data.Array.Repa.IO.BMP (writeImageToBMP)
import Data.Word (Word8)

import Img

-- Unboxed concrete values indexed by (x, y, channel)
type Image = Array Repa.U DIM2 (Word8, Word8, Word8)

type ImagePixel = (Word8, Word8, Word8)

-- Delayed values of (r, g, b, a) ready to be rendered
type Render = Array Repa.D DIM2 RenderPixel

type RenderPixel = (Double, Double, Double, Double)

raw :: (Int, Int) -> Array Repa.U DIM3 Double
raw (w, h) = Repa.fromListUnboxed (Z :. w :. h :. 4) (take (w * h * 4) (cycle [0]))

canvas :: (Int, Int) -> Render
canvas (w, h) = Repa.traverse (raw (w, h)) packDims packPixel where
    packDims (Z :. w :. h :. c) = (Z :. w :. h)
    packPixel indx (Z :. x :. y) =
      ( indx (Z :. x :. y :. 0)
      , indx (Z :. x :. y :. 1)
      , indx (Z :. x :. y :. 2)
      , indx (Z :. x :. y :. 3))

blend :: RenderPixel -> RenderPixel -> RenderPixel
blend (br, bg, bb, ba) (tr, tg, tb, ta) = (fc br tr, fc bg tg, fc bb tb, denom) where
        denom = ta + tb * (1 - ta)
        fc b t = ta * t + ba * b * (1 - ta) / (ta + tb * (1 - ta))

mapRGB :: (Double -> Double) -> RenderPixel -> RenderPixel
mapRGB f (r, g, b, a) = (f r, f g, f b, a)

black :: RenderPixel
black = (0, 0, 0, 1)

toImagePixel :: RenderPixel -> RenderPixel -> ImagePixel
toImagePixel bg p = let (r, g, b, _) = mapRGB (* 255) (blend bg p) in (floor r, floor g, floor b)

render :: RenderPixel -> Render -> Image
render bg r = Repa.computeS $ Repa.map (toImagePixel bg) r

main :: IO ()
main = writeImageToBMP "out.bmp" $ render black $ canvas (500, 500)
