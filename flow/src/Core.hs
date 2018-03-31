module Core
  ( Generate(..)
  , World(..)
  , Context(..)
  , cairo
  , sample
  , screen
  ) where

import Control.Monad.Reader
import Control.Monad.State
import Data.IORef
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
  , timeoutAdd
  , widgetGetDrawWindow
  , widgetGetDrawWindow
  , widgetShowAll
  , windowNew
  , windowSetDefaultSize
  )

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

sample :: RVar a -> State PureMT a
sample = sampleRVar

realize :: IORef Int -> PureMT -> World -> Generate () -> IO (Render ())
realize frameRef rng world work = do
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
  drawingArea <- drawingAreaNew
  containerAdd window drawingArea
  rng <- newPureMT
  frameRef <- newIORef 0
  let work' = realize frameRef rng (World width height seed factor) work
  drawingArea `onExpose` (\_ -> renderToScreen drawingArea work')
  timeoutAdd (renderToScreen drawingArea work') 16
  window `onDestroy` mainQuit
  windowSetDefaultSize window scaledWidth scaledHeight
  widgetShowAll window
  mainGUI
  where
    scaledHeight = round $ (fromIntegral height) * factor
    scaledWidth = round $ (fromIntegral width) * factor

renderToScreen :: DrawingArea -> IO (Render ()) -> IO Bool
renderToScreen da work = do
  work <- work
  dw <- widgetGetDrawWindow da
  renderWithDrawable dw work
  return True
