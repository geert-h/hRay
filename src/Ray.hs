module Ray (Ray (..), trace) where

import Color
import HitInfo (HitInfo (..))
import Linearisible (Linearisible (..))
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
distanceToSphere (Sphere (Vector3 cx cy cz) radius _) (Ray pos@(Vector3 x y z) dir@(Vector3 dx dy dz))
  | d > 0 = min (max 0 ((-b) + sqrt d / (2 * a))) (max 0 ((-b) - sqrt d / (2 * a)))
  | otherwise = 1e20
  where
    a = dir `dot` dir
    b = 2 * (dx * (x - cx) + dy * (y - cy) + dz * (z - cz))
    c = ((x - cx) * (x - cx) + (y - cy) * (y - cy) + (z - cz) * (z - cz)) - (radius * radius)
    d = (b * b) - (4 * a * c)

distanceToSphere' :: Sphere -> Ray -> Double
distanceToSphere' (Sphere centre radius _) (Ray origin direction) =
  if det < 0 then 1e20 else f
  where
    eps = 1e-4
    op = centre - origin
    b = op `dot` direction
    det = b * b - dot op op + radius * radius
    sdet = sqrt det
    a = b - sdet
    s = b + sdet
    f
      | a > eps = a
      | s > eps = s
      | otherwise = 1e20

cast :: World -> Ray -> Maybe HitInfo
cast world ray@(Ray origin direction) = case foldr closetSphere (Nothing, 1e20) (spheres world) of
  (Nothing, _) -> Nothing
  (Just (Sphere centerPos _ material), dist) ->
    let newPos = origin + direction .* dist
        newNormal = normalize $ newPos - centerPos
     in Just $ HitInfo newPos newNormal material
  where
    closetSphere sphere (oldSphere, oldDist) =
      let ndist = distanceToSphere sphere ray
       in if ndist < oldDist && ndist > 0
            then (Just sphere, ndist)
            else (oldSphere, oldDist)

trace :: Int -> World -> Ray -> IO Color
trace depth world ray@(Ray _ direction) = case cast world ray of
  Nothing -> return 0
  Just (HitInfo hitPos hitNormal (Material hitColor hitEmissionColor hitRefType)) -> do
    let depth' = depth + 1
        pr = maxL hitColor
        continue clr = case hitRefType of
          Diffuse -> do
            newDir <- diffuseDirection hitNormal
            newTrace <- trace depth' world (Ray hitPos newDir)
            return $ hitEmissionColor + (clr * newTrace)
          Specular -> do
            let newDir = specularDirection hitNormal direction
            newTrace <- trace depth' world (Ray hitPos newDir)
            return $ hitEmissionColor + (clr * newTrace)
          Normal -> do
            return $ Color (x hitNormal + 1) (y hitNormal + 1) (z hitNormal + 1) .* 0.5
          Refractive -> do undefined
    if depth' > 5
      then do
        rand <- randomValue
        if rand < pr
          then continue $ hitColor ./ pr
          else return hitEmissionColor
      else continue hitColor

-- trace _ _ (Ray _ dir) = let (Vector3 x y z) = normalize dir in return $ Color (x + 1) (y + 1) (z + 1) .* 0.5

diffuseDirection :: Vector3 -> IO Vector3
diffuseDirection normal = do
  randomDir <- randomDirection
  pure $ normalize $ randomDir + normal

specularDirection :: Vector3 -> Vector3 -> Vector3
specularDirection normal direction = direction - (normal .* (normal `dot` direction)) .* 2

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