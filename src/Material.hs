module Material (Material (..)) where

import Color (Color)

data Material = Material
  { color :: Color,
    emissionColor :: Color,
    smoothness :: Double,
    specularStrength :: Double
  }
  deriving (Eq, Show)