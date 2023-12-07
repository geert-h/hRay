module HitInfo where

import Material
import Vector3

data HitInfo = HitInfo
  { position :: Vector3,
    material :: Material,
    normal :: Vector3
  }