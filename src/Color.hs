module Color
  ( RGBA(..)
  , Dot(..)
  , Blender(..)
  , collapseColor
  , emptyColor
  , standardBlender
  ) where

import Data.Word (Word8)

data RGBA = RGBA
  { red :: Double
  , green :: Double
  , blue :: Double
  , alpha :: Double
  } deriving (Eq, Show)

emptyColor = RGBA {red = 0, green = 0, blue = 0, alpha = 0}

type Dot = (Word8, Word8, Word8)

type Blender = (RGBA -> RGBA -> RGBA)

standardBlender :: RGBA -> RGBA -> RGBA
standardBlender (RGBA { red = bgred
                      , green = bggreen
                      , blue = bgblue
                      , alpha = bgalpha
                      }) (RGBA { red = fgred
                               , green = fggreen
                               , blue = fgblue
                               , alpha = fgalpha
                               }) =
  RGBA
  { red = blend fgred bgred
  , green = blend fggreen bggreen
  , blue = blend fgblue bgblue
  , alpha = fgalpha + bgalpha * (1 - fgalpha)
  }
  where
    blend f b = f * fgalpha + b * (1 - fgalpha)

canvas = RGBA {red = 1, green = 1, blue = 1, alpha = 1}

collapseColor :: RGBA -> Dot
collapseColor fg = (f red, f green, f blue)
  where
    RGBA {red, green, blue, ..} = standardBlender canvas fg
    f = floor . (* 255)
