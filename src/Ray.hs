module Ray (Ray (..), trace) where

import Color
import HitInfo (HitInfo (..))
import Material (Material (..), ReflectionType (..))
import Sphere (Sphere (Sphere, centerPosition, material))
import System.Random (Random (randomR), newStdGen)
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

trace :: Int -> World -> Ray -> IO Color
trace depth world ray@(Ray origin direction) = case cast world ray of
  Nothing -> pure $ fromDouble 0.1
  Just (HitInfo hitPos (Material col emissionCol refType) hitNormal) -> do
    let continue f = case refType of
          Diffuse -> do
            newDir <- diffuseDirection hitNormal
            newTrace <- trace (depth + 1) world (Ray hitPos newDir)
            return $ emissionCol + (f * newTrace)
          Specular -> do
            let newDir = specularDirection hitNormal direction
            newTrace <- trace (depth + 1) world (Ray hitPos newDir)
            return $ emissionCol + (f * newTrace)
          Refractive -> do undefined
    if depth + 1 > 5
      then do
        let maxColVal = maxColorValue col
        rand <- randomValue
        if rand < maxColVal
          then continue $ col `mulColorByScalar` (1 / maxColVal)
          else return emissionCol
      else continue col

diffuseDirection :: Vector3 -> IO Vector3
diffuseDirection normal = do
  randomDir <- randomDirection
  pure $ normalize $ randomDir + normal

specularDirection :: Vector3 -> Vector3 -> Vector3
specularDirection normal direction = direction - normal `mulByScalar` (2 * normal `dot` direction)

randomValue :: IO Double
randomValue = do
  rand <- newStdGen
  let (value, _) = randomR (0.0, 1.0) rand
  return value

randomValueNormal :: IO Double
randomValueNormal = do
  value <- randomValue
  value' <- randomValue
  return $ sqrt (-2.0 * log value) * cos (2.0 * pi * value')

randomDirection :: IO Vector3
randomDirection = do
  x <- randomValueNormal
  y <- randomValueNormal
  z <- randomValueNormal
  return $ normalize (Vector3 x y z)

distanceToSphere :: Sphere -> Ray -> Double
distanceToSphere (Sphere (Vector3 cx cy cz) radius _) (Ray (Vector3 x y z) (Vector3 dx dy dz)) = if d < 0 then -1 else min (max 0 ((-b) + sqrt d / (2 * a))) (max 0 ((-b) - sqrt d / (2 * a)))
  where
    a = dx ** 2 + dy ** 2 + dz ** 2
    b = 2 * (dx * (x - cx) + dy * (y - cy) + dz * (z - cz))
    c = (x - cx) ** 2 + (y - cy) ** 2 + (z - cz) ** 2 - radius ** 2
    d = b ** 2 - 4 * a * c