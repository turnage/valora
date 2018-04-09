module Core
  ( Generate(..)
  , Random(..)
  , World(..)
  , Context(..)
  , cairo
  , screen
  , hsva
  , file
  , timeSeed
  , runInvocation
  , runRand
  ) where

import Control.Monad.Reader
import Control.Monad.State as State
import Data.Colour.RGBSpace.HSV
import Data.Colour.SRGB
import Data.IORef
import Data.RVar
import Data.Random
import Data.Random.Distribution.Normal
import Data.Random.Distribution.Uniform
import Data.Random.RVar
import Data.Random.Source.PureMT
import Data.Time.Clock.POSIX
import Graphics.Rendering.Cairo as Cairo
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Gdk.Events
import Graphics.UI.Gtk.OpenGL.Config
import Graphics.UI.Gtk.OpenGL.DrawingArea
import Numeric.Noise.Perlin
import Options

type Generate a = StateT PureMT (ReaderT Context Render) a

type Random a = State PureMT a

data World = World
  { width :: Int
  , height :: Int
  , seed :: Int
  , scaleFactor :: Double
  } deriving (Eq, Show)

data Context = Context
  { world :: World
  , frame :: Int
  , noise :: Perlin
  }

data MainOptions = MainOptions
  { optWidth :: Int
  , optHeight :: Int
  , optSave :: String
  , optScale :: Double
  , optSeed :: Int
  , optFrames :: Int
  }

instance Options MainOptions where
  defineOptions =
    pure MainOptions <*> simpleOption "w" 500 "Piece width." <*>
    simpleOption "h" 500 "Piece height." <*>
    simpleOption "o" "" "Save location." <*>
    simpleOption "s" 1 "Scale factor." <*>
    simpleOption "e" 0 "Rng seed." <*>
    simpleOption "f" 30 "Number of frames to save to file."

runRand :: Random a -> Generate a
runRand rand = do
  rng <- State.get
  let (val, rng') = runState rand rng
  State.put rng'
  return val

runInvocation :: Generate () -> IO ()
runInvocation scene =
  runCommand $ \opts args -> do
    seed <-
      if (optSeed opts) == 0
        then timeSeed
        else return $ optSeed opts
    let world = World (optWidth opts) (optHeight opts) seed (optScale opts)
    if optSave opts == ""
      then screen world scene
      else file (optSave opts) (optFrames opts) world scene

hsva :: Double -> Double -> Double -> Double -> Generate ()
hsva hue saturation value alpha =
  cairo $ setSourceRGBA channelRed channelGreen channelBlue alpha
  where
    RGB {..} = hsv hue saturation value

timeSeed :: IO (Int)
timeSeed = getPOSIXTime >>= \t -> return $ round . (* 1000) $ t

cairo :: Render a -> Generate a
cairo = lift . lift

preprocess :: IORef Int -> IORef Int -> World -> Generate () -> IO (Render ())
preprocess frameRef seedRef world work = do
  frame <- readIORef frameRef
  nextSeed <- readIORef seedRef
  modifyIORef frameRef (+ 1)
  let world' = world {seed = nextSeed}
  let rng = pureMT $ fromInteger $ toInteger nextSeed
  noise <-
    do octaves <- sampleRVar $ uniform 1 5
       persistance <- sampleRVar $ normal 2 1
       noiseScale <- sampleRVar $ normal 10 3
       let noise = perlin nextSeed octaves noiseScale persistance
       return noise
  let ctx = Context world (frame - 1) noise
  return $
    (flip runReaderT ctx) . (>>= (return . fst)) . (flip runStateT rng) $ do
      cairo $ do
        scale (scaleFactor world') (scaleFactor world')
        setAntialias AntialiasBest
      work

screen :: World -> Generate () -> IO ()
screen (World width height seed factor) work = do
  initGUI
  window <- windowNew
  glCfg <- glConfigNew [GLModeRGBA, GLModeDouble]
  drawingArea <- glDrawingAreaNew glCfg
  containerAdd window drawingArea
  seedRef <- newIORef seed
  putStrLn $ "Initial seed is: " ++ (show seed)
  frameRef <- newIORef 0
  let work' = preprocess frameRef seedRef (World width height seed factor) work
  timeoutAdd (renderToScreen scaledWidth scaledHeight drawingArea work') 16
  window `onKeyPress` ui frameRef seedRef
  window `onDestroy` mainQuit
  windowSetDefaultSize window scaledWidth scaledHeight
  widgetShowAll window
  mainGUI
  where
    scaledHeight = round $ (fromIntegral height) * factor
    scaledWidth = round $ (fromIntegral width) * factor

file :: String -> Int -> World -> Generate () -> IO ()
file path frames (World width height seed factor) work = do
  frameRef <- newIORef 0
  seedRef <- newIORef seed
  putStrLn $ "Output seed is: " ++ (show seed)
  let work' = preprocess frameRef seedRef (World width height seed factor) work
  let frame i = do
        workFrame <- work'
        surface <- createImageSurface FormatARGB32 scaledWidth scaledHeight
        renderWith surface workFrame
        surfaceWriteToPNG surface (path ++ "__" ++ (show i) ++ ".png")
  sequence $ map (frame) [0 .. frames]
  return ()
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

ui :: IORef Int -> IORef Int -> Event -> IO Bool
ui frameRef seedRef (Key {eventKeyVal, ..}) = do
  case eventKeyVal of
    65307 -> mainQuit
    114 -> do
      modifyIORef frameRef (const 0)
      seed <- timeSeed
      putStrLn $ "New seed is: " ++ (show seed)
      modifyIORef seedRef $ const seed
    _ -> return ()
  return True
ui _ _ _ = return True
