module Color
  ( HSVA(..)
  , hsva
  , RGBA(..)
  , Blender(..)
  , emptyColor
  , collapseColor
  , standardBlender
  ) where

import Data.Fixed (mod')
import Data.List (find)
import Data.Word (Word8)

data HSVA = HSVA
  { hue :: Double
  , saturation :: Double
  , value :: Double
  , alpha :: Double
  }

data RGBA = RGBA
  { red :: Double
  , green :: Double
  , blue :: Double
  , alpha :: Double
  }

type Blender = (RGBA -> RGBA -> RGBA)

emptyColor :: RGBA
emptyColor = RGBA {red = 0, green = 0, blue = 0, alpha = 0}

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

collapseColor :: RGBA -> (Word8, Word8, Word8)
collapseColor color = (f red, f green, f blue)
  where
    RGBA {red, green, blue, ..} = standardBlender canvas color
    f = floor . (* 255)

hsva :: HSVA -> RGBA
hsva HSVA {hue, saturation, value, alpha} = RGBA {red, green, blue, alpha}
  where
    chroma = value * saturation
    hue' = hue / 60
    x = chroma * (1 - (abs ((hue' `mod'` 2) - 1)))
    (r, g, b) =
      case floor hue' of
        0 -> (chroma, x, 0)
        1 -> (x, chroma, 0)
        2 -> (0, chroma, x)
        3 -> (0, x, chroma)
        4 -> (x, 0, chroma)
        5 -> (chroma, 0, x)
        _ -> (0, 0, 0)
    m = value - chroma
    (red, green, blue) = (r + m, g + m, b + m)
