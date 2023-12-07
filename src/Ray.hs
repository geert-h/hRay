module Ray where

import Color
import Debug.Trace (traceShowId)
import HitInfo (HitInfo (..))
import Material (Material (color))
import Sphere (Sphere (Sphere, centerPosition, material))
import System.Random (Random (randomR), StdGen, newStdGen)
import Vector3
import World

data Ray = Ray
  { position :: Vector3,
    direction :: Vector3
  }
  deriving (Eq, Show)

cast :: World -> Ray -> Maybe HitInfo
cast world ray = case foldr closetSphere (Nothing, -1) (spheres world) of
  (Nothing, _) -> Nothing
  (Just sphere, dist) -> Just $ HitInfo newPos (Sphere.material sphere) (normalize $ newPos - centerPosition sphere)
    where
      newPos = Ray.position ray + direction ray `mulByScalar` dist
  where
    closetSphere sphere (oldSphere, oldDist)
      | (ndist < oldDist || oldDist < 0) && ndist > 0 = (Just sphere, ndist)
      | otherwise = (oldSphere, oldDist)
      where
        ndist = distanceToSphere sphere ray

trace :: Int -> Color -> World -> Ray -> Color
trace 0 _ _ _ = Color 0 0 0 -- Maximum nr of bounces is met
trace bounces clr world ray = case cast world ray of
  Nothing -> Color 0 0 0
  Just hitInfo -> color (HitInfo.material hitInfo) + trace (bounces - 1) clr world (Ray (HitInfo.position hitInfo) (normal hitInfo))
  where
    specularOrDiffuse = undefined

randomValue :: IO Double
randomValue = do
  rand <- newStdGen
  let (value, _) = randomR (0.0, 1.0) rand
  return value

randomValueNormal :: IO Double
randomValueNormal = do
  value <- randomValue
  return $ sqrt (-2.0 * log value) * cos (2.0 * pi * value)

randomDirection :: IO Vector3
randomDirection = do
  x <- randomValueNormal
  y <- randomValueNormal
  normalize . Vector3 x y <$> randomValueNormal

distanceToSphere :: Sphere -> Ray -> Double
distanceToSphere (Sphere (Vector3 cx cy cz) radius _) (Ray (Vector3 x y z) (Vector3 dx dy dz)) = if d < 0 then -1 else min (max 0 ((-b) + sqrt d / (2 * a))) (max 0 ((-b) - sqrt d / (2 * a)))
  where
    a = dx ** 2 + dy ** 2 + dz ** 2
    b = 2 * (dx * (x - cx) + dy * (y - cy) + dz * (z - cz))
    c = (x - cx) ** 2 + (y - cy) ** 2 + (z - cz) ** 2 - radius ** 2
    d = b ** 2 - 4 * a * c

collidesWithSphere :: Sphere -> Ray -> Bool
collidesWithSphere s r = distanceToSphere s r > 0