module Ray (Ray (..), trace) where

import Color
import HitInfo (HitInfo (..))
import Material (Material (..), ReflectionType (..))
import Sphere (Sphere (Sphere))
import System.Random (Random (randomR), newStdGen)
import Vector3
import World

data Ray = Ray
  { position :: Vector3,
    direction :: Vector3
  }
  deriving (Eq, Show)

distanceToSphere :: Sphere -> Ray -> Double
distanceToSphere (Sphere (Vector3 cx cy cz) radius _) (Ray (Vector3 x y z) (Vector3 dx dy dz))
  | d > 0 = min (max 0 ((-b) + sqrt d / (2 * a))) (max 0 ((-b) - sqrt d / (2 * a)))
  | otherwise = -1
  where
    a = dx * dx + dy * dy + dz * dz
    b = 2 * (dx * (x - cx) + dy * (y - cy) + dz * (z - cz))
    c = (x - cx) * (x - cx) + (y - cy) * (y - cy) + (z - cz) * (z - cz) - (radius * radius)
    d = (b * b) - (4 * a * c)

cast :: World -> Ray -> Maybe HitInfo
cast world ray@(Ray position direction) = case foldr closetSphere (Nothing, -1) (spheres world) of
  (Nothing, _) -> Nothing
  (Just (Sphere centerPos _ material), dist) -> Just $ HitInfo newPos (normalize (newPos - centerPos)) material
    where
      newPos = position + direction `mulByScalar` dist
  where
    closetSphere sphere (oldSphere, oldDist)
      | (ndist < oldDist || oldDist < 0) && ndist > 0 = (Just sphere, ndist)
      | otherwise = (oldSphere, oldDist)
      where
        ndist = distanceToSphere sphere ray

trace :: Int -> World -> Ray -> IO Color
trace depth world ray@(Ray _ direction) = case cast world ray of
  Nothing -> pure $ fromDouble 0.1
  Just (HitInfo hitPos hitNormal (Material hitColor hitEmissionColor hitRefType)) -> do
    let depth' = depth + 1
        pr = maxColorValue hitColor
        continue clr = case hitRefType of
          Diffuse -> do
            newDir <- diffuseDirection hitNormal
            newTrace <- trace depth' world (Ray hitPos newDir)
            return $ hitEmissionColor + (clr * newTrace)
          Specular -> do
            let newDir = specularDirection hitNormal direction
            newTrace <- trace depth' world (Ray hitPos newDir)
            return $ hitEmissionColor + (clr * newTrace)
          Refractive -> do undefined
    if depth' > 5
      then do
        rand <- randomValue
        if rand < pr
          then continue $ hitColor `mulColorByScalar` (1 / pr)
          else return hitEmissionColor
      else continue hitColor

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