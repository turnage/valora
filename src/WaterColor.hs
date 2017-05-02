module WaterColor
  ( warp
  ) where

import Control.Lens (traverse, both, toListOf)
import qualified Data.HashMap as H
import Data.Traversable (mapAccumR)

import Img
import Rand
import Shape

waterColor
  :: Polygon p
  => Int -> Double -> Int -> Int -> p -> H.Map (Int, Int) RGBA
waterColor seed variance depth layers pol = H.unions $ map (\(pol, _) -> pol) warpedPolies
  where
    rng = gaussianBySeed seed variance
    warpedPolies = []

warp
  :: (Dist d, Polygon p)
  => d -> p -> (Irregular, d)
warp dist pol = (Irregular edges', dist')
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
