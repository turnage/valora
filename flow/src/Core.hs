module Core
  ( Generate(..)
  , World(..)
  , Context(..)
  , cairo
  , screen
  ) where

import Control.Monad.Reader
import Control.Monad.State
import Data.IORef
import Data.RVar
import Data.Random.Distribution.Normal
import Data.Random.RVar
import Data.Random.Source.PureMT
import Graphics.Rendering.Cairo as Cairo
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Gdk.Events
import Graphics.UI.Gtk.OpenGL.Config
import Graphics.UI.Gtk.OpenGL.DrawingArea

type Generate a = StateT PureMT (ReaderT Context Render) a

data World = World
  { width :: Int
  , height :: Int
  , seed :: Int
  , scaleFactor :: Double
  } deriving (Eq, Show)

data Context = Context
  { world :: World
  , frame :: Int
  }

frameRange :: World -> (Int, Int) -> Maybe [Context]
frameRange world (start, end) =
  if start < end
    then Just $ map (\i -> Context world i) [start .. end]
    else Nothing

cairo :: Render a -> Generate a
cairo = lift . lift

preprocess :: IORef Int -> PureMT -> World -> Generate () -> IO (Render ())
preprocess frameRef rng world work = do
  frame <- readIORef frameRef
  modifyIORef frameRef (+ 1)
  let ctx = Context world frame
  return $
    (flip runReaderT ctx) . (>>= (return . fst)) . (flip runStateT rng) $ do
      cairo $ scale (scaleFactor world) (scaleFactor world)
      work

screen :: World -> Generate () -> IO ()
screen (World width height seed factor) work = do
  initGUI
  window <- windowNew
  glCfg <- glConfigNew [GLModeRGBA, GLModeDouble]
  drawingArea <- glDrawingAreaNew glCfg
  containerAdd window drawingArea
  rng <- newPureMT
  frameRef <- newIORef 0
  let work' = preprocess frameRef rng (World width height seed factor) work
  timeoutAdd (renderToScreen scaledWidth scaledHeight drawingArea work') 16
  window `onKeyPress` ui
  window `onDestroy` mainQuit
  windowSetDefaultSize window scaledWidth scaledHeight
  widgetShowAll window
  mainGUI
  where
    scaledHeight = round $ (fromIntegral height) * factor
    scaledWidth = round $ (fromIntegral width) * factor

renderToScreen :: Int -> Int -> GLDrawingArea -> IO (Render ()) -> IO Bool
renderToScreen width height da work = do
  work <- work
  dw <- widgetGetDrawWindow da
  surface <- createImageSurface FormatARGB32 width height
  renderWith surface work
  renderWithDrawable dw $ do
    setSourceSurface surface 0 0
    Cairo.rectangle 0 0 (fromIntegral width) (fromIntegral height)
    fill
  return True

escapeKey :: KeyVal
escapeKey = 65307

ui :: Event -> IO Bool
ui Key {eventKeyVal, ..} =
  case eventKeyVal of
    escapeKey -> do
      mainQuit
      return True
    _ -> return True
ui _ = return True
