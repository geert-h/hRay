module Material (Material (..), ReflectionType (..)) where

import Color (Color)

data Material = Material
  { color :: !Color,
    emissionColor :: !Color,
    -- smoothness :: Double,
    -- specularStrength :: Double
    refType :: !ReflectionType
  }
  deriving (Eq, Show)

data ReflectionType = Diffuse | Specular | Refractive
  deriving (Eq, Show)
