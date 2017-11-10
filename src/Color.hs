{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}

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
  { red = bgred + fgred
  , green = bggreen + fggreen
  , blue = bgblue + fgblue
  , alpha = fgalpha + bgalpha * (1 - fgalpha)
  }

collapseColor :: RGBA -> Dot
collapseColor RGBA {red, green, blue, ..} = (f red, f green, f blue)
  where
    f = floor . (* 255)
