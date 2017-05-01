module Draw
  ( draw
  ) where

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
  , canvas :: Layer
  , color :: RGBA
  }

draw
  :: Polygon p
  => RGBA -> Layer -> p -> Layer
draw color layer pol =
  let (DrawContext {canvas = result}) =
        until
          drawDone
          drawStep
          DrawContext
          { activeEdges = V.filter (inScanRange sl) edges
          , remainingEdges = V.filter (not . inScanRange sl) edges
          , scanLine = sl
          , canvas = layer
          , color = color
          }
  in result
  where
    edges =
      V.modify
        (V.sortBy (\(ScanEdge {low = l1}) (ScanEdge {low = l2}) -> compare l1 l2))
        (scanEdges pol)
    ScanEdge {low = sl} = V.head edges

drawDone :: DrawContext -> Bool
drawDone (DrawContext {activeEdges = ae}) = V.null ae

drawStep :: DrawContext -> DrawContext
drawStep (DrawContext { activeEdges = ae
                      , remainingEdges = re
                      , scanLine = sl
                      , canvas = cv
                      , color = col
                      }) =
  DrawContext
  { activeEdges = activeEdges'
  , remainingEdges = V.filter (not . inScanRange (sl')) re
  , scanLine = sl + 1
  , canvas = canvas'
  , color = col
  }
  where
    sl' = sl + 1
    (canvas', _) =
      until
        (\(_, ls) -> null ls)
        (\(layer, (x1, x2):xs) -> (fillRow col (sl, x1, x2) layer, xs))
        (cv, overlaps sl (intersects sl ae))
    activeEdges' =
      V.modify
        (V.sortBy
           (\(ScanEdge {slope = slope1}) (ScanEdge {slope = slope2}) ->
              compare (slope1 sl') (slope2 sl')))
        (V.filter (inScanRange (sl')) (V.concat [re, ae]))

inScanRange :: Int -> ScanEdge -> Bool
inScanRange sl (ScanEdge {low = low, high = high}) = sl >= low && sl <= high

intersects :: Int -> V.Vector ScanEdge -> V.Vector Int
intersects sl es = V.map (\(ScanEdge {slope = slope}) -> slope sl) es

overlaps :: Int -> V.Vector Int -> [(Int, Int)]
overlaps sl xs =
  if length xs < 2
    then []
    else [(V.head xs, V.head (V.tail xs))] ++ overlaps sl (V.tail xs)

scanEdge :: Edge -> ScanEdge
scanEdge (Edge ((Point (x1, y1)), (Point (x2, y2)))) =
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
