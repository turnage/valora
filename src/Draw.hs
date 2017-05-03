module Draw
  ( draw
  ) where

import qualified Data.HashMap as H
import qualified Data.Vector as V
import qualified Data.Vector.Algorithms.Merge as V

import Img
import Shape

data ScanEdge = ScanEdge
  { low :: Int
  , high :: Int
  , slope :: Int -> Int
  }

data DrawContext = DrawContext
  { activeEdges :: V.Vector ScanEdge
  , remainingEdges :: V.Vector ScanEdge
  , scanLine :: Int
  , canvas :: H.Map (Int, Int) RGBA
  , color :: RGBA
  }

draw
  :: Polygon p
  => RGBA -> p -> H.Map (Int, Int) RGBA
draw color pol =
  let (DrawContext {canvas = result}) =
        until
          drawDone
          drawStep
          DrawContext
          { activeEdges = V.filter (inScanRange scanLine) edges
          , remainingEdges = V.filter (not . inScanRange scanLine) edges
          , scanLine = scanLine
          , canvas = H.empty
          , color = color
          }
  in result
  where
    ScanEdge {low = scanLine} = V.head edges
    edges =
      V.modify
        (V.sortBy (\(ScanEdge {low = l1}) (ScanEdge {low = l2}) -> compare l1 l2))
        (scanEdges pol)

drawDone :: DrawContext -> Bool
drawDone (DrawContext {activeEdges = ae}) = V.null ae

drawStep :: DrawContext -> DrawContext
drawStep (DrawContext { activeEdges = activeEdges
                      , remainingEdges = remainingEdges
                      , scanLine = scanLine
                      , canvas = canvas
                      , color = color
                      }) =
  DrawContext
  { activeEdges = activeEdges'
  , remainingEdges = V.filter (not . inScanRange (scanLine')) remainingEdges
  , scanLine = scanLine'
  , canvas = H.union canvas $ H.unions $ map H.unions $ map fillSegment segmentsToFill
  , color = color
  }
  where
    scanLine' = scanLine + 1
    segmentsToFill = overlaps $ intersects scanLine activeEdges
    fillSegment (x1, x2) = map (fillPixel) [x1 .. x2]
    fillPixel x = H.insert (x, scanLine) color H.empty
    activeEdges' =
      V.modify
        (V.sortBy
           (\(ScanEdge {slope = slope1}) (ScanEdge {slope = slope2}) ->
              compare (slope1 scanLine') (slope2 scanLine')))
        (V.filter (inScanRange (scanLine')) (V.concat [remainingEdges, activeEdges]))

inScanRange :: Int -> ScanEdge -> Bool
inScanRange scanLine (ScanEdge {low = low, high = high}) = scanLine >= low && scanLine <= high

intersects :: Int -> V.Vector ScanEdge -> V.Vector Int
intersects scanLine es = V.map (\(ScanEdge {slope = slope}) -> slope scanLine) es

overlaps :: V.Vector Int -> [(Int, Int)]
overlaps xs =
  if length xs < 2
    then []
    else [(V.head xs, V.head (V.tail xs))] ++ overlaps (V.tail xs)

scanEdge :: Edge -> ScanEdge
scanEdge (Edge {start = Point { x = x1, y = y1}, end = Point { x = x2, y = y2}}) =
  ScanEdge
  { low = starty
  , high = max y1 y2
  , slope = (\y -> startx + (floor (m * (fromIntegral (y - starty)))))
  }
  where
    starty = min y1 y2
    startx =
      if starty == y1
        then x1
        else x2
    deltaX = fromIntegral $ x2 - x1
    deltaY = fromIntegral $ y2 - y1
    m =
      if deltaY == 0
        then 0
        else (deltaX / deltaY)

scanEdges
  :: Polygon p
  => p -> V.Vector ScanEdge
scanEdges pol = V.map (scanEdge) $ V.fromList $ edges pol
