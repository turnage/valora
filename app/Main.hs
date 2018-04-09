module Main where

import Control.Monad.Reader
import Control.Monad.State
import Data.Maybe
import Data.Random.Distribution.Normal
import qualified Data.Vector as V
import Linear

import Core
import Geom
import Scenes.CopyLines
import Stroke
import Wiggle

main :: IO ()
main = do
  runInvocation scene
