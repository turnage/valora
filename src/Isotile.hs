module Isotile
  ( Isotile(..)
  , IsoSide(..)
  , isoSideMask
  , isotileGrid
  , isotileGridMask
  ) where

import Control.Lens ((^.))
import Control.Monad.Reader
import Data.Fixed (mod')
import Data.Maybe
import qualified Data.Vector as V
import Graphics.Rendering.Cairo
  ( Operator(..)
  , fill
  , getOperator
  , rectangle
  , setOperator
  , setSourceRGBA
  )
import Linear
import Linear.V2

import Compositing
import Coords
import Core
import Geom

data Isotile = Isotile
  { a :: V2 Double
  , b :: V2 Double
  , c :: V2 Double
  , period :: Double
  } deriving (Eq, Show)

data IsoSide
  = IsoLeft
  | IsoCenter
  | IsoRight
  deriving (Eq, Show)

isoSideMask :: IsoSide -> Isotile -> Contour
isoSideMask side (Isotile a b c period) =
  let d = midpoint a c
      e = V2 (a ^. _x) (a ^. _y + period / 2)
      f = V2 (c ^. _x) (c ^. _y + period / 2)
      g = V2 (d ^. _x) (d ^. _y + period)
  in case side of
       IsoLeft -> Contour $ V.fromList [a, b, d, e]
       IsoCenter -> Contour $ V.fromList [e, d, f, g]
       IsoRight -> Contour $ V.fromList [b, c, f, d]

shift :: V2 Double -> Isotile -> Isotile
shift off (Isotile a b c p) = Isotile (a + off) (b + off) (c + off) p

isotiles :: Double -> [Isotile]
isotiles period = map (tile) [0 ..]
  where
    f x = abs ((x `mod'` period) - period / 2)
    tile i =
      Isotile
      { a =
          let x = i * period
          in V2 x (f x)
      , b =
          let x = i * period + period / 2
          in V2 x (f x)
      , c =
          let x = (i + 1) * period
          in V2 x (f x)
      , period
      }

isotileGrid :: Double -> Generate [Isotile]
isotileGrid period = do
  World {width, height, ..} <- asks world
  let neededColumns = 2 + (round $ (fromIntegral height) / period)
  let neededRows = 1 + (round $ (fromIntegral width) / period)
  let rows = map (const $ isotiles period) [0 .. neededRows]
  let rows' = map (\(i, row) -> map (shifter i) row) $ zip [0 ..] rows
  return $ concat $ map (take neededColumns) rows'
  where
    shifter row =
      let y = fromIntegral row * period - period
      in if row `mod` 2 == 0
           then shift $ V2 (-period) y
           else shift $ V2 ((period / 2) - period) y

isotileGridMask ::
     Double -> (Generate (), Generate (), Generate ()) -> Generate ()
isotileGridMask period (left, center, right) = do
  alphaMatte (isotileMaskLayer period IsoLeft) left
  alphaMatte (isotileMaskLayer period IsoCenter) center
  alphaMatte (isotileMaskLayer period IsoRight) right

isotileMaskLayer :: Double -> IsoSide -> Generate ()
isotileMaskLayer period side = do
  grid <- isotileGrid period
  World {width, height, ..} <- asks world
  cairo $ setSourceRGBA 1 0 0 1
  foldr1 (>>) $
    map (>> cairo fill) $ mapMaybe (drawContour . (isoSideMask side)) grid
