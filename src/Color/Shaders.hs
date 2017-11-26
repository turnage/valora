module Color.Shaders
  ( Shader
  , staticFill
  ) where

import Color (RGBA(..))
import Coords (Point(..))

type Shader = (Point -> RGBA)

staticFill :: RGBA -> Shader
staticFill color = const color
