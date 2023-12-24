module Sphere (Sphere (..)) where

import Material
import Vector3

data Sphere = Sphere
  { centerPosition :: !Vector3,
    radius :: !Double,
    material :: {-# UNPACK #-} !Material
  }
  deriving (Eq, Show)