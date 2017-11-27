module Transformers.WaterColor
  ( waterColor
  ) where

import qualified Data.Vector as V

import Color (RGBA(..))
import Color.Shaders (Shader(..))
import Poly (Poly(..))
import Poly.Properties (Extent(..), extent)
import Rand (SampleFeed(..), gaussianBySeed, Dist(..))
import Transformers.Warp (warp, adaptiveWarp)
import VectorUtil (enumerate)

waterColor :: Int
           -> Int
           -> Int
           -> Double
           -> Shader
           -> Poly
           -> V.Vector (Shader, Poly)
waterColor seed layers depth spread shader poly = V.zip shaders polies
  where
    polies =
      V.map (\(i, (warper, poly)) -> warper (SampleFeed (drop i feed)) poly) $
      enumerate $ V.zip warpers rawPolies
    rawPolies = V.generate layers $ const $ poly
    shaders = V.zipWith (fadeShader) proportions rawShaders
    fadeShader fade shader point = color {alpha = alpha'}
      where
        alpha' = (alpha color) * 0.2 * fade
        color = shader point
    rawShaders = V.generate layers $ const $ shader
    warpers = V.generate layers $ const $ adaptiveWarp depth (spread * size)
    proportions = V.generate layers (\i -> fromIntegral i / fromIntegral layers)
    SampleFeed feed = randFeed $ gaussianBySeed seed spread
    size = maximum [height, width]
    Extent {height, width, ..} = extent poly
