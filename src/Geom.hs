module Geom
  ( Contour(..)
  , Line(..)
  , drawContour
  , square
  , line
  ) where

import Control.Lens
import qualified Data.List as L
import qualified Data.Vector as V
import Graphics.Rendering.Cairo (closePath, fill, lineTo, moveTo, newPath)
import Linear

import Coords
import Core
import Traits.Meta
import Traits.Position
import Traits.Transform

newtype Contour =
  Contour (V.Vector (V2 Double))

newtype Line =
  Line (V.Vector (V2 Double))

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

instance Subdivide Contour where
  subdivide (Contour vertices) = Contour $ _subdivide True vertices

instance Subdivide Line where
  subdivide (Line vertices) = Line $ _subdivide False vertices

instance Geom2D Contour where
  vertices (Contour vertices) = vertices
  fromVertices = Contour

instance Geom2D Line where
  vertices (Line vertices) = vertices
  fromVertices = Line

_subdivide :: Bool -> V.Vector (V2 Double) -> V.Vector (V2 Double)
_subdivide wrap vertices =
  V.fromList $ concat $ L.transpose [original, midpoints]
  where
    midpoints = V.toList $ V.map (uncurry midpoint) $ _edges wrap vertices
    original = V.toList vertices

_edges :: Bool -> V.Vector (V2 Double) -> V.Vector (V2 Double, V2 Double)
_edges wrap vertices =
  if wrap
    then V.zip vertices $ V.snoc (V.tail vertices) $ V.head vertices
    else V.zip vertices $ V.tail vertices

line :: Int -> V2 Double -> V2 Double -> V.Vector (V2 Double)
line n start end = V.generate n (grade)
  where
    grade i = start + (factor i * delta)
    factor i = (fromIntegral i) / (fromIntegral (n - 1))
    delta = end - start

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
