module HitInfo (HitInfo (..)) where

import Material
import Vector3

data HitInfo = HitInfo
  { position :: Vector3,
    normal :: Vector3,
    material :: Material
  }
  deriving (Show, Eq)