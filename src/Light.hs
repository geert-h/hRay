module Light (Light (..)) where

import Color
import Vector3

data Light = Light
  { radius :: Double,
    location :: Vector3,
    color :: Color
  }