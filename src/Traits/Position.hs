module Traits.Position
  ( Translate(..)
  , Centered(..)
  , Place(..)
  ) where

import Linear

import Coords

class Centered c where
  centroid :: c -> V2 Double

class Translate t where
  translate :: V2 Double -> t -> t

class Place p where
  place :: V2 Double -> p -> p

instance (Translate a, Centered a) => Place a where
  place dest a = translate (dest - centroid a) a
