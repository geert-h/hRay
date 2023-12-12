module Vector3 where

data Vector3 = Vector3
  { x :: {-# UNPACK #-} !Double,
    y :: {-# UNPACK #-} !Double,
    z :: {-# UNPACK #-} !Double
  }
  deriving (Eq, Show)

instance Num Vector3 where
  (+) (Vector3 x y z) (Vector3 x' y' z') = Vector3 (x + x') (y + y') (z + z')
  (-) (Vector3 x y z) (Vector3 x' y' z') = Vector3 (x - x') (y - y') (z - z')
  (*) (Vector3 x y z) (Vector3 x' y' z') = Vector3 (x * x') (y * y') (z * z')
  abs v = Vector3 (abs $ x v) (abs $ y v) (abs $ z v)
  negate v = Vector3 (-x v) (-y v) (-z v)
  signum v = Vector3 (signum $ x v) (signum $ y v) (signum $ z v)
  fromInteger v = Vector3 (fromInteger v) (fromInteger v) (fromInteger v)

maxVec :: Vector3 -> Double
maxVec v = max (x v) (max (y v) (z v))

dot :: Vector3 -> Vector3 -> Double
(Vector3 x y z) `dot` (Vector3 x' y' z') = x * x' + y * y' + z * z'

size :: Vector3 -> Double
size (Vector3 x y z) = sqrt $ x * x + y * y + z * z

cross :: Vector3 -> Vector3 -> Vector3
(Vector3 x y z) `cross` (Vector3 x' y' z') = Vector3 (y * z' - z * y') (z * x' - x * z') (x * y' - y * x')

divByScalar :: Vector3 -> Double -> Vector3
(Vector3 x y z) `divByScalar` s = Vector3 (x / s) (y / s) (z / s)

mulByScalar :: Vector3 -> Double -> Vector3
(Vector3 x y z) `mulByScalar` s = Vector3 (x * s) (y * s) (z * s)

normalize :: Vector3 -> Vector3
normalize v = v `divByScalar` size v

safeNormal :: Vector3 -> Maybe Vector3
safeNormal v =
  let len = size v
   in if len /= 0.0
        then Just $ v `divByScalar` len
        else Nothing