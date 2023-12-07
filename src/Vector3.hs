module Vector3 where

data Vector3 = Vector3
  { x :: Double,
    y :: Double,
    z :: Double
  }
  deriving (Eq, Show)

instance Num Vector3 where
  (+) v1 v2 = Vector3 (x v1 + x v2) (y v1 + y v2) (z v1 + z v2)
  (-) v1 v2 = Vector3 (x v1 - x v2) (y v1 - y v2) (z v1 - z v2)
  (*) v1 v2 = Vector3 (x v1 * x v2) (y v1 * y v2) (z v1 * z v2)
  abs v = Vector3 (abs $ x v) (abs $ y v) (abs $ z v)
  negate v = Vector3 (-x v) (-y v) (-z v)
  signum v = Vector3 (signum $ x v) (signum $ y v) (signum $ z v)
  fromInteger v = Vector3 (fromInteger v) (fromInteger v) (fromInteger v)

dot :: Vector3 -> Vector3 -> Double
l `dot` r = x l * x r + y l * y r + z l * z r

size :: Vector3 -> Double
size v = sqrt $ v `dot` v

cross :: Vector3 -> Vector3 -> Vector3
l `cross` r = Vector3 (y l * z r - z l * y r) (z l * x r - x l * z r) (x l * y r - y l * x r)

divByScalar :: Vector3 -> Double -> Vector3
v `divByScalar` s = Vector3 (x v / s) (y v / s) (z v / s)

mulByScalar :: Vector3 -> Double -> Vector3
v `mulByScalar` s = Vector3 (x v * s) (y v * s) (z v * s)

normalize :: Vector3 -> Vector3
normalize v = v `divByScalar` size v

safeNormal :: Vector3 -> Maybe Vector3
safeNormal v =
  let len = size v
   in if len /= 0.0
        then Just $ v `divByScalar` len
        else Nothing