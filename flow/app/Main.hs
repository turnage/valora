module Main where

import Control.Monad.Reader
import Data.Colour.RGBSpace
import Data.Colour.RGBSpace.HSV
import Data.Maybe
import Data.Random.Distribution.Normal
import Data.Random.Source.PureMT
import Graphics.Rendering.Cairo

import Coords
import Core
import Flow
import Geom
import Isotile

world_ :: World
world_ = World 500 500 1 2

main :: IO ()
main =
  screen world_ $ do
    sqs <-
      flow
        (FlowCfg (normal 50 10) (normal 50 10) (normal 0.2 1))
        (square (Point 0 0) 2)
    Context (World width height _ _) frame <- ask
    cairo $ do
      setSourceRGBA 1 1 1 1
      rectangle 0 0 (fromIntegral width) (fromIntegral height)
      fill
      setSourceRGBA 0 0 0 1
    foldr1 (>>) $ mapMaybe (drawContour) $ map (\f -> f frame) sqs

scene :: Generate ()
scene = do
  Context _ frame <- ask
  let left =
        cairo $ setSourceRGBA 0 0 (fromIntegral (frame `mod` 100) / 100.0) 1
  let center = cairo $ setSourceRGBA 0 1 0 1
  let right = cairo $ setSourceRGBA 1 0 0 1
  isotileGridMask 80 (left, center, right)
