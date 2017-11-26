module Raster.Poly.Scan
  ( scanRaster
  ) where

import Data.List (maximumBy, minimumBy)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Vector as V

import Color
import Color.Shaders
import Constants (rasterSize, pixelSize)
import Poly
import Poly.Properties
import Raster (Raster(..), emptyRaster, rasterWith)

indexSet :: (a -> Bool) -> V.Vector a -> S.Set Int
indexSet predicate vec =
  S.filter (\i -> predicate $ vec V.! i) $ S.fromList [0 .. (V.length vec - 1)]

data Slope
  = Slope { m :: Double
         ,  b :: Double}
  | Vertical Double
  deriving (Eq, Show)

data ScanEdge = ScanEdge
  { highPoint :: Double
  , lowPoint :: Double
  , slope :: Slope
  } deriving (Show)

inScanLine :: Double -> ScanEdge -> Bool
inScanLine scanLine ScanEdge {highPoint, lowPoint, ..} =
  scanLine >= lowPoint && scanLine < highPoint

passedBy :: Point -> ScanEdge -> Bool
passedBy Point {x, y} (ScanEdge {slope, ..}) =
  case slope of
    Slope {m, b} -> (y - b) / m < x
    Vertical staticX -> staticX < x

-- Maybe construct a scan Edge. We don't want edges with a flat slope because
-- they are useless.
fromEdge :: Edge -> Maybe ScanEdge
fromEdge (Edge {start, end}) =
  if y delta == 0
    then Nothing
    else Just ScanEdge {lowPoint = y lowPoint, highPoint = y highPoint, slope}
  where
    slope =
      if x delta == 0
        then Vertical $ x lowPoint
        else Slope {m, b}
      where
        b = (y lowPoint) - ((x lowPoint) * m)
        m = (y delta) / (x delta)
    highPoint = maximumBy (compareHeight) [start, end]
    lowPoint = minimumBy (compareHeight) [start, end]
    delta = highPoint - lowPoint
    compareHeight p1 p2 = compare (y p1) (y p2)

scanRaster :: Blender -> V.Vector (Shader, Poly) -> Raster
scanRaster blender polies = rasterWith (pixelColor)
  where
    pixelColor point = V.foldl (blender) emptyColor $ colorStack point
    colorStack point = V.map (uncurry (shadePixel point)) polies
    shadePixel point shader poly = color {alpha = alpha'}
      where
        alpha' = (alpha color) * opacity
        color = shader point
        opacity =
          (fromIntegral $ V.length $ V.filter (id) samples) /
          (fromIntegral $ V.length samples)
        samples = V.map (inScan poly) $ superSample point
    inScan poly Point {x, y} =
      odd $ S.size $ S.intersection passedEdges activeEdges
      where
        passedEdges = indexSet (passedBy Point {x, y}) _edges
        activeEdges = indexSet (inScanLine y) _edges
        _edges = V.mapMaybe (fromEdge) $ edges poly

superSample :: Point -> V.Vector Point
superSample point =
  V.concatMap ((radialSuperSample point) . (pixelSize /) . (2 ^)) $
  V.fromList [1 .. 4]

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
