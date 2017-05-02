module WaterColor
  ( warp
  , waterColor
  ) where

import Control.Lens (traverse, both, toListOf)
import qualified Data.HashMap as H
import Data.Traversable (mapAccumR)

import Draw
import Img
import Rand
import Shape

waterColor
  :: Polygon p
  => Int -> Double -> Int -> Int -> RGBA -> p -> H.Map (Int, Int) RGBA
waterColor seed variance depth layers (r, g, b, a) pol = H.unions drawings
  where
    opacityFrac i = (1 - (fromIntegral i) / (fromIntegral layers)) * a
    varianceFrac i = ((fromIntegral i) / (fromIntegral layers)) * variance
    distOf i = gaussianBySeed (seed * i) $ varianceFrac i
    drawings = map (\(i, pol) -> draw (r, g, b, opacityFrac i) pol) $ zip [0 ..] warpedPolies
    (_, warpedPolies) =
      unzip $
      map (\i -> (iterate (uncurry warp) (warp (distOf i) pol)) !! (depth - 1)) $ take layers [1 ..]

warp
  :: (Dist d, Polygon p)
  => d -> p -> (d, Irregular)
warp dist pol = (dist', Irregular edges')
  where
    edges' = toListOf (traverse . both) edgePairs
    (dist', edgePairs) = mapAccumR (warpEdge) dist (edges pol)

midpoint :: Edge -> Point
midpoint (Edge (Point (x1, y1), Point (x2, y2))) =
  Point (x1 + (x2 - x1) `div` 2, y1 + (y2 - y1) `div` 2)

warpEdge
  :: Dist d
  => d -> Edge -> (d, (Edge, Edge))
warpEdge dist (Edge (p1, p2)) = (dist', (Edge (p1, pm), Edge (pm, p2)))
  where
    (pm, dist') = warpPoint dist $ midpoint (Edge (p1, p2))

warpPoint
  :: Dist d
  => d -> Point -> (Point, d)
warpPoint dist (Point (x, y)) = (Point (x + floor xoffset, y + floor yoffset), dist')
  where
    ((xoffset, yoffset), dist') = randPair dist
