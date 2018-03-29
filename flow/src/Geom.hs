module Geom
  ( Contour(..)
  , drawContour
  ) where

import qualified Data.Vector as V
import Graphics.Rendering.Cairo

import Coords

newtype Contour =
  Contour (V.Vector Point)

drawContour :: Contour -> Maybe (Render ())
drawContour (Contour vertices) =
  if V.length vertices >= 3
    then Just $ foldr1 (>>) $ concat [initCmds, lines, endCmds]
    else Nothing
  where
    initCmds = [newPath, moveTo (Coords.x start) (Coords.y start)]
    lines = V.toList $ V.map (\(Point x y) -> lineTo x y) $ V.tail vertices
    endCmds = [closePath]
    start = V.head vertices
