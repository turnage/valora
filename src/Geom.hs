module Geom
  ( Contour(..)
  , drawContour
  , square
  ) where

import Control.Lens
import qualified Data.Vector as V
import Graphics.Rendering.Cairo (closePath, fill, lineTo, moveTo, newPath)
import Linear

import Coords
import Core
import Traits.Position

newtype Contour =
  Contour (V.Vector (V2 Double))

instance Translate Contour where
  translate delta (Contour vertices) = Contour (V.map (+ delta) vertices)

instance Centered Contour where
  centroid (Contour vertices) = V2 ((right + left) / 2) ((top + bottom) / 2)
    where
      left = V.minimum xs
      right = V.maximum xs
      top = V.maximum ys
      bottom = V.minimum ys
      ys = V.map (^. _y) vertices
      xs = V.map (^. _x) vertices

square :: V2 Double -> Double -> Contour
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
    initCmds = [newPath, moveTo (startX) (startY)]
    lines = V.toList $ V.map (\(V2 x y) -> lineTo x y) $ V.tail vertices
    endCmds = [closePath]
    V2 startX startY = V.head vertices
