module Raster.Poly.Scan
  ( scanRaster
  ) where

import Data.List (maximumBy, minimumBy)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Vector as V

import Data.Maybe

import Color (RGBA(..))
import Color.Shaders
import Constants (rasterSize, pixelSize)
import Coords (Pixel(..), Point(..), Subrange(..), fromRasterCoord)
import Poly
import Poly.Properties (Edge(..), extent, extentCoords, edges)
import Raster (Raster(..), emptyRaster, rasterWith)
import Raster.Mask (Mask(..))
import VectorUtil (enumerate)

indexSet :: (a -> Bool) -> V.Vector a -> S.Set Int
indexSet predicate vec =
  S.filter (\i -> predicate $ vec V.! i) $ S.fromList [0 .. (V.length vec - 1)]

data Slope
  = Slope { m :: Double
         ,  b :: Double}
  | Vertical Double
  deriving (Eq, Show)

data ScanEdge = ScanEdge
  { high :: Double
  , low :: Double
  , slope :: Slope
  } deriving (Show)

inScanLine :: Double -> ScanEdge -> Bool
inScanLine scanLine ScanEdge {high, low, ..} =
  scanLine >= low && scanLine < high

passedBy :: Point -> ScanEdge -> Bool
passedBy Point {x, y} (ScanEdge {slope, ..}) =
  case slope of
    Slope {m, b} -> (y - b) / m < x
    Vertical staticX -> staticX < x

-- Maybe construct a scan Edge. We don't want edges with a flat slope because
-- they are useless.
fromEdge :: Edge -> Maybe ScanEdge
fromEdge (Edge {start, end}) =
  if yDelta == 0
    then Nothing
    else Just ScanEdge {low, high, slope}
  where
    slope =
      if xDelta == 0
        then Vertical $ lowX
        else Slope {m, b}
      where
        b = low - (lowX * m)
        m = yDelta / xDelta
    Point {x = highX, y = high} = maximumBy (compareHeight) [start, end]
    Point {x = lowX, y = low} = minimumBy (compareHeight) [start, end]
    (xDelta, yDelta) = (highX - lowX, high - low)
    compareHeight Point {y = y1, x = _} Point {y = y2, x = _} = compare y1 y2

scanRaster :: Shader -> Poly -> Mask
scanRaster shader poly = mask {subrange = colors}
  where
    colors = V.map (uncurry shadePixel) $ enumerate coords
    coords = subrange mask
    shadePixel i point = color {alpha = alpha'}
      where
        alpha' = (alpha color) * opacity
        color = shader point
        opacity =
          (fromIntegral $ V.length $ V.filter (id) samples) /
          (fromIntegral $ V.length samples)
          where
            inScan i point =
              odd $ V.length $ V.filter (passedBy point) activeEdges
            activeEdges =
              fromMaybe V.empty $ rowTables V.!? (i `mod` (height mask))
            samples = V.map (inScan i) $ superSample point
    scanEdges = V.mapMaybe (fromEdge) $ edges poly
    mask = extentCoords $ extent poly
    rowTables = V.map (active) ys
    active y = V.filter (inScanLine y) scanEdges
    ys = V.generate (height mask) (fromRasterCoord . (+ startY))
    Pixel {y = startY, x = _} = bottomLeft mask

superSample :: Point -> V.Vector Point
superSample point =
  V.concatMap ((radialSuperSample point) . (pixelSize /)) $ V.fromList [1 .. 4]

radialSuperSample :: Point -> Double -> V.Vector Point
radialSuperSample Point {x, y} offset =
  V.fromList
    [ Point {x = x + offset, y}
    , Point {x, y = y + offset}
    , Point {x = x - offset, y}
    , Point {x, y = y - offset}
    , Point {x = x + offset, y = y + offset}
    , Point {x = x - offset, y = y + offset}
    , Point {x = x + offset, y = y - offset}
    , Point {x = x - offset, y = y - offset}
    ]
