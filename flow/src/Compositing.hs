module Compositing
  ( alphaMatte
  ) where

import Control.Monad.Reader
import Graphics.Rendering.Cairo

import Core

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
