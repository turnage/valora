module Color.Shaders
  ( Shader
  , staticFill
  ) where

import Color
import Poly

type Shader = (Point -> RGBA)

staticFill :: RGBA -> Shader
staticFill color = const color
