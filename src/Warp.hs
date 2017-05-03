module Warp
  ( Warper(..)
  , warpDupe
  , warpPoly
  ) where

import Control.Lens (traverse, both, toListOf)
import Data.Traversable (mapAccumR)

import Rand
import Shape

data Warper d = Warper
  { dist :: d
  , heat :: (Point -> Double)
  }

warpDupe
  :: (Dist d, Polygon p)
  => Warper d -> Int -> Int -> p -> (Warper d, [Irregular])
warpDupe warper depth layers poly = (warper', warpedPolies)
  where
    (warper', warpedPolies) = mapAccumR (deepWarpPoly depth) warper $ take layers $ repeat poly

deepWarpPoly
  :: (Dist d, Polygon p)
  => Int -> Warper d -> p -> (Warper d, Irregular)
deepWarpPoly n warper poly = iterate (uncurry warpPoly) (warper, (Irregular (edges poly))) !! n

warpPoly
  :: (Dist d, Polygon p)
  => Warper d -> p -> (Warper d, Irregular)
warpPoly warper poly = (warper', Irregular edges')
  where
    edges' = toListOf (traverse . both) edgePairs
    (warper', edgePairs) = mapAccumR (warpEdge) warper (edges poly)

warpEdge
  :: Dist d
  => Warper d -> Edge -> (Warper d, (Edge, Edge))
warpEdge warper (Edge (p1, p2)) = (warper', (Edge (p1, pw), Edge (pw, p2)))
  where
    (warper', pw) = warpPoint warper $ midpoint p1 p2

warpPoint
  :: Dist d
  => Warper d -> Point -> (Warper d, Point)
warpPoint (Warper {dist = dist, heat = heat}) (Point (x, y)) = (warper', point')
  where
    warper' = Warper {dist = dist', heat = heat}
    point' = Point (x + floor (xshift * strength), y + floor (yshift * strength))
    strength = heat $ Point (x, y)
    ((xshift, yshift), dist') = randPair dist

midpoint :: Point -> Point -> Point
midpoint (Point (x1, y1)) (Point (x2, y2)) = Point (x1 + (x2 - x1) `div` 2, y1 + (y2 - y1) `div` 2)
