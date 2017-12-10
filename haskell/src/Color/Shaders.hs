module Color.Shaders
  ( Shader
  , staticFill
  , translucer
  ) where

import Color (RGBA(..))
import Coords (Point(..))

type Shader = (Point -> RGBA)

staticFill :: RGBA -> Shader
staticFill color = const color

translucer :: Shader -> Double -> Shader
translucer shader opacityFactor = tuneOpacity . shader
  where
    tuneOpacity color = color {alpha = (alpha color) * opacityFactor}
