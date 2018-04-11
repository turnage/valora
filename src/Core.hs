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
import qualified Data.Vector as V
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

scaledDimensions :: World -> (Int, Int)
scaledDimensions World {width, height, scaleFactor, ..} =
  ( round $ fromIntegral width * scaleFactor
  , round $ fromIntegral height * scaleFactor)

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
  , optBrainstorm :: Bool
  }

instance Options MainOptions where
  defineOptions =
    pure MainOptions <*> simpleOption "w" 500 "Piece width." <*>
    simpleOption "h" 500 "Piece height." <*>
    simpleOption "o" "" "Save location." <*>
    simpleOption "s" 1 "Scale factor." <*>
    simpleOption "e" 0 "Rng seed." <*>
    simpleOption "f" 1 "Number of frames to save to file." <*>
    simpleOption "b" False "Brainstorm mode."

data RenderContext = RenderContext
  { renderSeed :: IORef Int
  , renderFrame :: IORef Int
  , renderedFrames :: IORef (V.Vector Surface)
  , renderWorld :: World
  , renderScene :: Generate ()
  , renderEndFrame :: Int
  , renderBrainstorm :: Bool
  }

data RenderJob = RenderJob
  { canvas :: IO Surface
  , dimensions :: (Int, Int)
  , ctx :: RenderContext
  }

mkRender :: World -> Generate () -> Int -> Bool -> IO RenderJob
mkRender world scene endFrame brainstorm = do
  ctx' <- ctx
  return
    RenderJob
    {canvas = render ctx', dimensions = scaledDimensions world, ctx = ctx'}
  where
    ctx = do
      seedRef <- newIORef (seed world)
      frameRef <- newIORef 0
      renderedFramesRef <- newIORef V.empty
      return
        RenderContext
        { renderSeed = seedRef
        , renderFrame = frameRef
        , renderedFrames = renderedFramesRef
        , renderScene = scene
        , renderEndFrame = endFrame
        , renderWorld = world
        , renderBrainstorm = brainstorm
        }

render :: RenderContext -> IO Surface
render RenderContext { renderSeed
                     , renderFrame
                     , renderedFrames
                     , renderWorld
                     , renderScene
                     , renderEndFrame
                     , renderBrainstorm
                     } = do
  frame <- readIORef renderFrame
  modifyIORef renderFrame $ (`mod` renderEndFrame) . (+ 1)
  rendered <- readIORef renderedFrames
  if V.length rendered > frame
    then return $ rendered V.! frame
    else do
      surface <- _render frame
      modifyIORef renderedFrames (\frames -> V.snoc frames surface)
      return surface
  where
    (scaledWidth, scaledHeight) = scaledDimensions renderWorld
    _render frame = do
      seed <-
        if renderBrainstorm
          then do
            r <- getRandomDouble >>= return . round . (* 1000000)
            modifyIORef renderSeed (const r)
            return r
          else readIORef renderSeed
      let world' = renderWorld {seed}
      let rng = pureMT $ fromInteger $ toInteger seed
      noise <-
        do octaves <- sampleRVar $ uniform 1 5
           persistance <- sampleRVar $ normal 2 1
           noiseScale <- sampleRVar $ normal 10 3
           let noise = perlin seed octaves noiseScale persistance
           return noise
      let ctx = Context world' frame noise
      surface <- createImageSurface FormatARGB32 scaledWidth scaledHeight
      renderWith surface $
        (flip runReaderT ctx) . (>>= (return . fst)) . (flip runStateT rng) $ do
          cairo $ do
            scale (scaleFactor renderWorld) (scaleFactor renderWorld)
            setAntialias AntialiasBest
          renderScene
      return surface

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
    render <- mkRender world scene (optFrames opts) (optBrainstorm opts)
    putStrLn $ "Initial seed is: " ++ (show seed)
    if optSave opts == ""
      then screen render
      else file (optSave opts) render

hsva :: Double -> Double -> Double -> Double -> Generate ()
hsva hue saturation value alpha =
  cairo $ setSourceRGBA channelRed channelGreen channelBlue alpha
  where
    RGB {..} = hsv hue saturation value

timeSeed :: IO (Int)
timeSeed = getPOSIXTime >>= \t -> return $ round . (* 1000) $ t

cairo :: Render a -> Generate a
cairo = lift . lift

screen :: RenderJob -> IO ()
screen RenderJob { canvas
                 , dimensions
                 , ctx = RenderContext {renderSeed, renderFrame, renderedFrames}
                 } = do
  initGUI
  window <- windowNew
  glCfg <- glConfigNew [GLModeRGBA, GLModeDouble]
  drawingArea <- glDrawingAreaNew glCfg
  containerAdd window drawingArea
  timeoutAdd (renderToScreen drawingArea canvas dimensions) 16
  window `onKeyPress` ui renderFrame renderSeed renderedFrames
  window `onDestroy` mainQuit
  uncurry (windowSetDefaultSize window) dimensions
  widgetShowAll window
  mainGUI

file :: String -> RenderJob -> IO ()
file path RenderJob { canvas
                    , dimensions
                    , ctx = RenderContext {renderEndFrame, renderSeed, ..}
                    , ..
                    } = do
  let writeFrame i = do
        surface <- canvas
        seed <- readIORef renderSeed
        surfaceWriteToPNG
          surface
          (path ++ "__" ++ (show seed) ++ "__" ++ (show i) ++ ".png")
  sequence $ map (writeFrame) [0 .. renderEndFrame - 1]
  return ()

renderToScreen :: GLDrawingArea -> IO Surface -> (Int, Int) -> IO Bool
renderToScreen da surface (width, height) = do
  dw <- widgetGetDrawWindow da
  surface' <- surface
  renderWithDrawable dw $ do
    setSourceSurface surface' 0 0
    Cairo.rectangle 0 0 (fromIntegral width) (fromIntegral height)
    fill
  return True

ui :: IORef Int -> IORef Int -> IORef (V.Vector Surface) -> Event -> IO Bool
ui frameRef seedRef renderedFramesRef (Key {eventKeyVal, ..}) = do
  case eventKeyVal of
    65307 -> mainQuit
    114 -> do
      modifyIORef renderedFramesRef (const V.empty)
      modifyIORef frameRef (const 0)
      newSeed <- timeSeed
      putStrLn $ "New seed is: " ++ (show newSeed)
      modifyIORef seedRef $ const newSeed
    _ -> return ()
  return True
ui _ _ _ _ = return True
