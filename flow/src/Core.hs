module Core
  ( Generate(..)
  , World(..)
  , cairo
  , sample
  , realize
  , screen
  ) where

import Control.Monad.Reader
import Control.Monad.State
import Data.RVar
import Data.Random.Distribution.Normal
import Data.Random.RVar
import Data.Random.Source.PureMT
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

type Generate a = StateT PureMT (ReaderT World Render) a

data World = World
  { width :: Int
  , height :: Int
  , seed :: Int
  , scaleFactor :: Double
  } deriving (Eq, Show)

cairo :: Render a -> Generate a
cairo = lift . lift

sample :: RVar a -> State PureMT a
sample = sampleRVar

realize :: World -> PureMT -> Generate () -> Render ()
realize world rng work =
  (flip runReaderT world) . (>>= (return . fst)) . (flip runStateT rng) $ do
    cairo $ scale (scaleFactor world) (scaleFactor world)
    work

screen :: World -> Render () -> IO ()
screen (World width height _ factor) work = do
  initGUI
  window <- windowNew
  drawingArea <- drawingAreaNew
  containerAdd window drawingArea
  drawingArea `onExpose`
    (\_ -> renderToScreen drawingArea (work >> scale factor factor))
  window `onDestroy` mainQuit
  windowSetDefaultSize window scaledWidth scaledHeight
  widgetShowAll window
  mainGUI
  where
    scaledHeight = round $ (fromIntegral height) * factor
    scaledWidth = round $ (fromIntegral width) * factor

renderToScreen :: DrawingArea -> Render () -> IO Bool
renderToScreen da work = do
  dw <- widgetGetDrawWindow da
  renderWithDrawable dw work
  return True
