module Main where

import Data.Colour.RGBSpace
import Data.Colour.RGBSpace.HSV
import Data.Maybe
import Graphics.Rendering.Cairo
import Graphics.UI.Gtk
  ( DrawingArea
  , containerAdd
  , drawingAreaNew
  , initGUI
  , mainGUI
  , mainQuit
  , onDestroy
  , onExpose
  , renderWithDrawable
  , widgetGetDrawWindow
  , widgetGetDrawWindow
  , widgetShowAll
  , windowNew
  , windowSetDefaultSize
  )

import Coords
import Geom
import Isotile

width_ :: Double
width_ = 500

height_ :: Double
height_ = 500

circle :: Double -> Point -> Render ()
circle radius (Point centerX centerY) =
  arc centerX centerY radius 0 (2 * pi) >> closePath

dots :: [Point] -> Render ()
dots points = foldr1 (>>) $ map (circle 1) points

isotileDots :: [Isotile] -> [Point]
isotileDots ((Isotile a b c _):is) = a : b : c : (isotileDots is)
isotileDots [] = []

main :: IO ()
main = do
  initGUI
  window <- windowNew
  drawingArea <- drawingAreaNew
  containerAdd window drawingArea
  drawingArea `onExpose` (\_ -> renderScene drawingArea)
  window `onDestroy` mainQuit
  windowSetDefaultSize window (round width_) (round height_)
  widgetShowAll window
  mainGUI

renderScene :: DrawingArea -> IO Bool
renderScene da = do
  dw <- widgetGetDrawWindow da
  renderWithDrawable dw $ do
    setSourceRGBA 1 0 0 1
    foldr1 (>>) $
      map (>> fill) $
      mapMaybe (drawContour . (isoSideMask IsoLeft)) $
      isotileGrid 80 width_ height_
    setSourceRGBA 0 0 1 1
    foldr1 (>>) $
      map (>> fill) $
      mapMaybe (drawContour . (isoSideMask IsoCenter)) $
      isotileGrid 80 width_ height_
    setSourceRGBA 0 1 0 1
    foldr1 (>>) $
      map (>> fill) $
      mapMaybe (drawContour . (isoSideMask IsoRight)) $
      isotileGrid 80 width_ height_
  return True
