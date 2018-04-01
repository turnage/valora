module Main where

import Control.Applicative
import Control.Monad.Reader
import Data.Colour
import Data.Colour.RGBSpace
import Data.Colour.RGBSpace.HSV
import Data.Maybe
import Data.RVar
import Data.Random.Distribution.Normal
import Data.Random.Source.PureMT
import Graphics.Rendering.Cairo
import Options

import Compositing
import Coords
import Core
import Flow
import Geom
import Isotile

data MainOptions = MainOptions
  { optWidth :: Int
  , optHeight :: Int
  , optSave :: String
  , optScale :: Double
  , optSeed :: Int
  , optFrames :: Int
  }

instance Options MainOptions where
  defineOptions =
    pure MainOptions <*> simpleOption "w" 500 "Piece width." <*>
    simpleOption "h" 500 "Piece height." <*>
    simpleOption "o" "" "Save location." <*>
    simpleOption "s" 1 "Scale factor." <*>
    simpleOption "e" 0 "Rng seed." <*>
    simpleOption "f" 30 "Number of frames to save to file."

world_ :: World
world_ = World 500 500 1 2

main :: IO ()
main =
  runCommand $ \opts args -> do
    seed <-
      if (optSeed opts) == 0
        then timeSeed
        else return $ optSeed opts
    let world = World (optWidth opts) (optHeight opts) seed (optScale opts)
    if optSave opts == ""
      then screen world scene
      else file (optSave opts) (optFrames opts) world scene

squares :: Double -> Generate () -> Generate ()
squares angle color = do
  sqSize <- sampleRVar $ normal 2 3
  let sqSize' = 1 + abs sqSize
  let density = normal (100 / sqSize') (100 / (sqSize' * 4))
  sqs <-
    flow
      (FlowCfg density density (normal 0.2 3) (normal angle 20))
      (square (Point 0 0) $ sqSize')
  Context (World width height _ _) frame <- ask
  color
  foldr1 (>>) $ mapMaybe (drawContour) $ map (\f -> f frame) sqs

scene :: Generate ()
scene = do
  let c1 = hsva 306 0.37 0.81 1
  let c2 = hsva 267 0.5 0.75 1
  let c3 = hsva 346 0.24 1 1
  let white = hsva 0 0 1 1
  period <- sampleRVar $ normal 60 20
  isotileGridMask period (solid c1, solid c2, solid c3)
  let left = squares 45 white
  let center = squares 0 white
  let right = squares 110 white
  cairo $ setOperator OperatorOverlay
  isotileGridMask period (left, center, right)
