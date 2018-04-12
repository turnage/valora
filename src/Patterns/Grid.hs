module Patterns.Grid
  ( GridCfg(..)
  , TileFocus(..)
  , grid
  , gridCfgDefault
  , gridCoords
  ) where

import Control.Monad.Reader
import Data.Maybe
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

-- row-order grid of points
grid :: GridCfg -> Generate [V2 Double]
grid GridCfg {cols, rows, width, height, topLeft, tileFocus} = do
  World {width = worldWidth, height = worldHeight, ..} <- asks world
  let (width', height') =
        ( fromMaybe (fromIntegral worldWidth) width
        , fromMaybe (fromIntegral worldHeight) height)
  let (tileWidth, tileHeight) =
        (width' / fromIntegral cols, height' / fromIntegral rows)
  let point (c, r) =
        V2 (fromIntegral c * tileWidth) (fromIntegral r * tileHeight)
  let tiles = map (point) $ gridCoords (cols, rows)
  return $ map ((+ topLeft) . (focalPoint tileWidth tileHeight tileFocus)) tiles

gridCoords :: (Int, Int) -> [(Int, Int)]
gridCoords (cols, rows) = [(c, r) | c <- [0 .. cols - 1], r <- [0 .. rows - 1]]
