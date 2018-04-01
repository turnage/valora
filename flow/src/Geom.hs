module Geom
  ( Contour(..)
  , drawContour
  , square
  ) where

import qualified Data.Vector as V
import Graphics.Rendering.Cairo (closePath, fill, lineTo, moveTo, newPath)

import Coords
import Core
import Traits.Position

newtype Contour =
  Contour (V.Vector Point)

instance Translate Contour where
  translate delta (Contour vertices) = Contour (V.map (+ delta) vertices)

instance Centered Contour where
  centroid (Contour vertices) = Point ((right + left) / 2) ((top + bottom) / 2)
    where
      left = V.minimum xs
      right = V.maximum xs
      top = V.maximum ys
      bottom = V.minimum ys
      ys = V.map (y) vertices
      xs = V.map (x) vertices

square :: Point -> Double -> Contour
square topLeft size =
  Contour
    (V.fromList
       [ topLeft
       , slideX topLeft size
       , slideX (slideY topLeft size) size
       , slideY topLeft size
       ])

drawContour :: Contour -> Maybe (Generate ())
drawContour (Contour vertices) =
  if V.length vertices >= 3
    then Just $ cairo $ path >> fill
    else Nothing
  where
    path = foldr1 (>>) $ concat [initCmds, lines, endCmds]
    initCmds = [newPath, moveTo (Coords.x start) (Coords.y start)]
    lines = V.toList $ V.map (\(Point x y) -> lineTo x y) $ V.tail vertices
    endCmds = [closePath]
    start = V.head vertices
