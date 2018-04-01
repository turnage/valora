module Compositing
  ( alphaMatte
  , solid
  ) where

import Control.Monad.Reader
import Graphics.Rendering.Cairo

import Core

solid :: Generate () -> Generate ()
solid color = do
  World {width, height, ..} <- asks world
  color
  cairo $ do
    rectangle 0 0 (fromIntegral width) (fromIntegral height)
    fill

alphaMatte :: Generate () -> Generate () -> Generate ()
alphaMatte matte src = do
  World width height _ _ <- asks world
  cairo $ pushGroup
  src
  cairo $ do
    popGroupToSource
    pushGroup
  matte
  cairo $ withGroupPattern mask
