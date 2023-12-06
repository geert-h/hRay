module Vector3 where

data Vector3 = Vector3
  { vX :: Double,
    vY :: Double,
    vZ :: Double
  }
  deriving (Eq, Show)

instance Num Vector3 where
  (+) :: Vector3 -> Vector3 -> Vector3
  (+) v1 v2 = Vector3 (vX v1 + vX v2) (vY v1 + vY v2) (vZ v1 + vZ v2)
  (-) :: Vector3 -> Vector3 -> Vector3
  (-) v1 v2 = Vector3 (vX v1 - vX v2) (vY v1 - vY v2) (vZ v1 - vZ v2)
  (*) :: Vector3 -> Vector3 -> Vector3
  (*) v1 v2 = Vector3 (vX v1 * vX v2) (vY v1 * vY v2) (vZ v1 * vZ v2)
  abs :: Vector3 -> Vector3
  abs v = Vector3 (abs $ vX v) (abs $ vY v) (abs $ vZ v)
  negate :: Vector3 -> Vector3
  negate v = Vector3 (-vX v) (-vY v) (-vZ v)
  signum :: Vector3 -> Vector3
  signum v = Vector3 (signum $ vX v) (signum $ vY v) (signum $ vZ v)
  fromInteger :: Integer -> Vector3
  fromInteger v = Vector3 (fromInteger v) (fromInteger v) (fromInteger v)