module Transformers.Wiggle
  ( WiggleCfg(..)
  , WiggleCoverage(..)
  , WiggleExpansion(..)
  , wiggle
  ) where

import Control.Monad.Random
import Data.Random.Normal
import qualified Data.Vector as V

import Coords (Point(..))
import Coords.Math (distance)
import Poly (Poly(..))
import Poly.Properties (centroid)

data WiggleCfg = WiggleCfg
  { adaptToNeighbors :: Bool
  , strength :: Double
  , coverage :: WiggleCoverage
  , expansion :: WiggleExpansion
  , shareSample :: Bool
  , spatialAdapter :: Maybe (Point -> Double)
  }

data WiggleCoverage
  = All
  | Odd

data WiggleExpansion
  = Outward
  | Inward
  | Neutral

wiggle
  :: RandomGen g
  => WiggleCfg -> Poly -> Rand g Poly
wiggle cfg Poly {vertices} = do
  seed <- getRandom
  return Poly {vertices = wiggleVertices cfg seed vertices}

wiggleVertices :: WiggleCfg -> Int -> V.Vector Point -> V.Vector Point
wiggleVertices WiggleCfg { adaptToNeighbors
                         , strength
                         , coverage
                         , expansion
                         , shareSample
                         , spatialAdapter
                         } seed vertices = V.update vertices updates
  where
    updates = V.mapMaybe (id) $ V.generate (V.length vertices) wiggler'
    neighbor i = vertices V.! (i `mod` (V.length vertices))
    wiggler' i =
      case (coverage, odd i) of
        (All, _) -> Just (i, wiggler i $ vertices V.! i)
        (Odd, True) -> Just (i, wiggler i $ vertices V.! i)
        _ -> Nothing
    wiggler i Point {x, y} = Point {x = x', y = y'}
      where
        x' =
          x +
          strength * (sign (x - xCenter) s1) * (adaptiveFactor i) *
          (spatialAdaptiveFactor i)
        y' =
          y +
          strength * (sign (y - yCenter) s2) * (adaptiveFactor i) *
          (spatialAdaptiveFactor i)
        (s1, s2) = samplePair i
    spatialAdaptiveFactor i =
      case spatialAdapter of
        Nothing -> 1
        Just spatialAdapterF -> spatialAdapterF $ vertices V.! i
    samplePair i =
      if shareSample
        then V.head normalPairs
        else normalPairs V.! i
    normalPairs =
      V.fromList $
      take (V.length vertices) $
      zipWith (,) normalFeed $ drop (V.length vertices) normalFeed
    normalFeed = mkNormals seed
    adaptiveFactor i =
      if adaptToNeighbors
        then distance (neighbor (i - 1)) (neighbor (i + 1))
        else 1
    Point {x = xCenter, y = yCenter} = centroid Poly {vertices}
    sign s v =
      case (expansion, s == abs s) of
        (Neutral, _) -> v
        (Outward, False) -> -1 * (abs v)
        (Outward, True) -> abs v
        (Inward, False) -> abs v
        (Inward, True) -> -1 * (abs v)
