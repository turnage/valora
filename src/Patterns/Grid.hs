module Patterns.Grid
  ( GridCfg(..)
  , TileFocus(..)
  , grid
  , gridCfgDefault
  ) where

import Control.Monad.Reader
import Linear

import Core

data TileFocus
  = Center
  | TopLeft
  | TopCenter
  | TopRight
  | BottomLeft
  | BottomRight

focalPoint :: Double -> Double -> TileFocus -> V2 Double -> V2 Double
focalPoint width height focus (V2 topLeftX topLeftY) =
  case focus of
    Center -> V2 (topLeftX + width / 2) (topLeftY + height / 2)
    TopLeft -> V2 topLeftX topLeftY
    TopCenter -> V2 (topLeftX + width / 2) topLeftY
    TopRight -> V2 (topLeftX + width) topLeftY
    BottomLeft -> V2 topLeftX (topLeftY + height)
    BottomRight -> V2 (topLeftX + width) (topLeftY + height)

data GridCfg = GridCfg
  { rows :: Int
  , cols :: Int
  , width :: Maybe Double
  , height :: Maybe Double
  , topLeft :: V2 Double
  , tileFocus :: TileFocus
  }

gridCfgDefault =
  GridCfg
  { rows = 10
  , cols = 10
  , width = Nothing
  , height = Nothing
  , topLeft = V2 0 0
  , tileFocus = Center
  }

-- row-order grid of points (top left corners)
grid :: GridCfg -> Generate [[V2 Double]]
grid GridCfg {rows, cols, width, height, topLeft, tileFocus} = do
  World {width = worldWidth, height = worldHeight, ..} <- asks world
  let width' =
        case width of
          Just w -> w
          Nothing -> fromIntegral worldWidth
  let height' =
        case height of
          Just h -> h
          Nothing -> fromIntegral worldHeight
  let tileWidth = width' / fromIntegral cols
  let tileHeight = height' / fromIntegral rows
  let point r c = V2 (fromIntegral c * tileWidth) (fromIntegral r * tileHeight)
  let tiles = map (\r -> map (\c -> point r c) [0 .. cols - 1]) [0 .. rows - 1]
  return $ (map . map) (focalPoint tileWidth tileHeight tileFocus) tiles
