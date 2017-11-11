module Scan
  ( preprocessPoly
  , scan
  ) where

import Color
import Poly
import Raster (Rasterable(..), RasterSize(..))

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

activeAt :: Double -> ScanEdge -> Bool
activeAt scanLine ScanEdge {highPoint, lowPoint, ..} = scanLine >= y lowPoint && scanLine <= y highPoint

passedBy :: Double -> ScanEdge -> Bool
passedBy x (ScanEdge {slope, ..}) = slope x < x

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

updateScangonX :: Point -> Scangon -> Scangon
updateScangonX point scangon =
  scangon {passedEdges = indexSet (passedBy $ x point) $ edgeSet scangon}

updateScangonY :: Double -> Scangon -> Scangon
updateScangonY y scangon =
  scangon {activeEdges = indexSet (activeAt y) $ edgeSet scangon}

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
  , blender :: Blender
  }

instance Show Scan where
  show scan = show $ scangons scan

instance Rasterable Scan where
  raster size scan = V.map (\y -> renderY size y scan) $ genCoordVector size

scan :: Blender -> V.Vector Scangon -> Scan
scan blender scangons = Scan {scangons, blender}

updateScanX :: Point -> Scan -> Scan
updateScanX point scan =
  scan {scangons = V.map (updateScangonX point) $ scangons scan}

updateScanY :: Double -> Scan -> Scan
updateScanY y scan =
  scan {scangons = V.map (updateScangonY y) $ scangons scan}

scanColor :: Scan -> RGBA
scanColor Scan {scangons, blender} =
  V.foldl (blender) (emptyColor) $ V.map (scangonColor) scangons

scanDone :: Scan -> Bool
scanDone scan = V.all (S.null . passedEdges) $ scangons scan

renderY :: RasterSize -> Double -> Scan -> V.Vector RGBA
renderY size y scan = V.map (renderPixel) $ genCoordVector size
  where
    fitCoords = (/ fromIntegral size) . fromIntegral
    renderPixel x = scanColor $ updateScanX Point {x, y} scan'
    scan' = updateScanY y scan

-- Returns a set of coordinates in one dimension with mappings from the raster pixel space to our
-- vector space which is 0-1, so 10 pixels yields the vector 0, 0.1, 0.2 ... 1
genCoordVector :: RasterSize -> V.Vector Double
genCoordVector size = V.map ((/ fromIntegral size) . fromIntegral) $ V.fromList [0..size]