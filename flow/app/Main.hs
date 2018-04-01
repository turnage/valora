module Main where

import Control.Applicative
import Control.Monad.Reader
import Data.Colour.RGBSpace
import Data.Colour.RGBSpace.HSV
import Data.Maybe
import Data.Random.Distribution.Normal
import Data.Random.Source.PureMT
import Graphics.Rendering.Cairo
import Options

import Coords
import Core
import Flow
import Geom
import Isotile

data MainOptions = MainOptions
  { optWidth :: Int
  , optHeight :: Int
  , optSave :: String
  }

instance Options MainOptions where
  defineOptions =
    pure MainOptions <*> simpleOption "w" 500 "Piece width." <*>
    simpleOption "h" 500 "Piece height." <*>
    simpleOption "s" "" "Save location."

world_ :: World
world_ = World 500 500 1 2

main :: IO ()
main =
  runCommand $ \opts args ->
    let world = World (optWidth opts) (optHeight opts) 1 1
    in do if optSave opts == ""
            then screen world scene
            else file (optSave opts) world scene

squares :: Double -> Generate ()
squares angle = do
  sqs <-
    flow
      (FlowCfg (normal 50 10) (normal 50 10) (normal 0.2 1) (normal angle 20))
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
  let left = squares 10
  let center = squares 45
  let right = squares 80
  isotileGridMask 80 (left, center, right)
