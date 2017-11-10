{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}

module Scan
  ( render
  , preprocessPoly
  , scan
  ) where

import Color
import Poly
import qualified Raster as R

import Control.Monad.State
import Data.List (maximumBy, minimumBy)
import qualified Data.Set as S
import qualified Data.Vector as V

indexSet :: (a -> Bool) -> V.Vector a -> S.Set Int
indexSet predicate vec =
  S.filter (\i -> predicate $ vec V.! i) $ S.fromList [0 .. (V.length vec - 1)]

data ScanEdge = ScanEdge
  { highPoint :: Point
  , lowPoint :: Point
  , slope :: Double -> Double
  }

instance Show ScanEdge where
  show edge = show (highPoint edge, lowPoint edge)

activeAt :: R.RasterSize -> Int -> ScanEdge -> Bool
activeAt size scanLine (ScanEdge {highPoint, lowPoint, ..}) =
  scanLine >= low && scanLine <= high
  where
    high = rasterHeight highPoint
    low = rasterHeight lowPoint
    rasterHeight = (R.toRasterCoord size) . y

passedBy :: R.RasterSize -> R.Pixel -> ScanEdge -> Bool
passedBy size (R.Pixel {pixelX, ..}) (ScanEdge {slope, ..}) = egdeX < pixelX
  where
    egdeX = R.toRasterCoord size $ slope $ R.fromRasterCoord size pixelX

-- Maybe construct a scan Edge. We don't want edges with a flat slope because
-- they are useless.
fromEdge :: Edge -> Maybe ScanEdge
fromEdge (Edge {start, end}) =
  if y delta == 0
    then Nothing
    else Just ScanEdge {highPoint, lowPoint, slope}
  where
    highPoint = maximumBy (compareHeight) [start, end]
    lowPoint = minimumBy (compareHeight) [start, end]
    slope y' = x lowPoint + ((y' - (y lowPoint)) * m)
    delta = highPoint - lowPoint
    m =
      if y delta == 0
        then 0
        else if x delta == 0
               then 0
               else (y delta) / (x delta)
    compareHeight p1 p2 = compare (y p1) (y p2)

data Scangon = Scangon
  { activeEdges :: S.Set Int
  , passedEdges :: S.Set Int
  , edgeSet :: V.Vector ScanEdge
  , fill :: RGBA
  } deriving (Show)

preprocessPoly
  :: Poly p
  => RGBA -> p -> Scangon
preprocessPoly fill poly =
  Scangon {activeEdges = S.empty, passedEdges = S.empty, edgeSet, fill}
  where
    edgeSet = V.mapMaybe (fromEdge) $ edges poly

startLine :: Scangon -> Double
startLine Scangon {edgeSet, ..} = V.minimum $ V.map (y . lowPoint) edgeSet

updateScangonColumn :: R.RasterSize -> R.Pixel -> Scangon -> Scangon
updateScangonColumn size pixel scangon =
  scangon {passedEdges = indexSet (passedBy size pixel) $ edgeSet scangon}

updateScangonRow :: R.RasterSize -> Int -> Scangon -> Scangon
updateScangonRow size row scangon =
  scangon {activeEdges = indexSet (activeAt size row) $ edgeSet scangon}

inScan :: Scangon -> Bool
inScan scangon =
  odd $ S.size $ S.intersection (activeEdges scangon) (passedEdges scangon)

-- TODO: add splatter
scangonColor :: Scangon -> RGBA
scangonColor scangon =
  if inScan scangon
    then fill scangon
    else emptyColor

data Scan = Scan
  { scangons :: V.Vector Scangon
  } deriving (Show)

scan :: V.Vector Scangon -> Scan
scan scangons = Scan {scangons}

updateScanColumn :: R.RasterSize -> R.Pixel -> Scan -> Scan
updateScanColumn size pixel scan =
  scan {scangons = V.map (updateScangonColumn size pixel) $ scangons scan}

updateScanRow :: R.RasterSize -> Int -> Scan -> Scan
updateScanRow size row scan =
  scan {scangons = V.map (updateScangonRow size row) $ scangons scan}

scanColor :: Blender -> Scan -> RGBA
scanColor blender scan =
  V.foldl (blender) (emptyColor) $ V.map (scangonColor) $ scangons scan

scanDone :: Scan -> Bool
scanDone scan = V.all (S.null . passedEdges) $ scangons scan

renderRow :: R.RasterSize -> Blender -> Int -> Scan -> V.Vector RGBA
renderRow size blender row scan = V.map (renderPixel) $ V.fromList [0 .. size]
  where
    renderPixel col =
      scanColor blender $ updateScanColumn size (pixel col) scan'
    pixel col = R.Pixel {pixelX = col, pixelY = row}
    scan' = updateScanRow size row scan

render :: R.RasterSize -> Blender -> Scan -> R.Layer
render size blender scan = R.mapColor (color) $ R.newLayer size
  where
    color (R.Pixel {pixelX, pixelY}) _ = (pixels V.! pixelY) V.! pixelX
    pixels =
      V.map (\row -> renderRow size blender row scan) $ V.fromList [0 .. size]
