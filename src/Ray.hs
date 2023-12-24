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
distanceToSphere (Sphere centre radius _) (Ray origin direction)
  | discriminant < 0 = 1e20
  | dst < 0 = 1e20
  | otherwise = dst
  where
    offsetRayOrigin = origin - centre
    a = dot direction direction
    b = 2 * dot offsetRayOrigin direction
    c = dot offsetRayOrigin offsetRayOrigin - radius * radius
    discriminant = b * b - 4 * a * c
    dst = (-b - sqrt discriminant) / (2 * a)

cast :: World -> Ray -> Maybe HitInfo
cast world ray@(Ray origin direction) = case foldr closetSphere (Nothing, 1e20) (spheres world) of
  (Nothing, _) -> Nothing
  (Just (Sphere centerPos _ material), dist) ->
    let newPos = origin + direction .* dist
        newNormal = normalize $ newPos - centerPos
     in Just $ HitInfo newPos newNormal material
  where
    closetSphere sphere o@(_, oldDist) =
      let ndist = distanceToSphere sphere ray
       in if ndist < oldDist
            then (Just sphere, ndist)
            else o

trace :: Int -> World -> Ray -> IO Color
trace depth world ray@(Ray _ direction) = case cast world ray of
  Nothing -> return 0
  Just (HitInfo hitPos hitNormal (Material hitColor hitEmissionColor hitRefType)) -> do
    let depth' = depth + 1
        continue clr = case hitRefType of
          Diffuse -> do
            newDir <- diffuseDirection hitNormal
            newTrace <- trace depth' world (Ray hitPos newDir)
            return $ hitEmissionColor + clr * newTrace
          Specular -> do
            let newDir = specularDirection hitNormal direction
            newTrace <- trace depth' world (Ray hitPos newDir)
            return $ hitEmissionColor + clr * newTrace
          Normal -> do
            return $ Color (x hitNormal + 1) (y hitNormal + 1) (z hitNormal + 1) .* 0.5
          Refractive -> do
            let nc = 1
                nt = 1.5
                nl = if dot hitNormal direction < 0 then hitNormal else negate hitNormal
                into = dot hitNormal nl > 0
                nnt = if into then nc / nt else nt / nc :: Double
                ddn = dot direction nl
                cos2t = 1 - nnt * nnt * (1 - ddn * ddn)
                reflRay = Ray hitPos (direction - hitNormal .* (2 * dot hitNormal direction))
            if cos2t < 0 -- Total internal reflection
              then do
                newTrace <- trace depth' world reflRay
                return $ hitEmissionColor + clr * newTrace
              else do
                let tdir = normalize (direction .* nnt - (hitNormal .* ((if into then 1 else -1) * (ddn * nnt + sqrt cos2t))))
                    a = nt - nc
                    b = nt + nc
                    r0 = a * a / (b * b)
                    c = 1 - if into then -ddn else dot tdir hitNormal
                    re = r0 + (1 - r0) * c * c * c * c * c
                    tr = 1 - re
                    p = 0.25 + 0.5 * re
                    rp = re / p
                    tp = tr / (1 - p)
                newTrace <-
                  if depth' > 2
                    then do
                      rand <- randomValue
                      if rand < p
                        then (.* rp) <$> trace depth' world reflRay
                        else (.* tp) <$> trace depth' world (Ray hitPos tdir)
                    else do
                      rad0 <- (.* re) <$> trace depth' world reflRay
                      rad1 <- (.* tr) <$> trace depth' world (Ray hitPos tdir)
                      return (rad0 + rad1)
                return $ hitEmissionColor + clr * newTrace
    if depth' > 5
      then do
        rand <- randomValue
        let pr = maxL hitColor
        if rand < pr
          then continue $ hitColor ./ pr
          else return hitEmissionColor
      else continue hitColor

-- trace _ _ (Ray _ dir) = let (Vector3 x y z) = normalize dir in return $ Color (x + 1) (y + 1) (z + 1) .* 0.5

diffuseDirection :: Vector3 -> IO Vector3
diffuseDirection normal = do
  randomDir <- randomDirection
  pure $ normalize $ randomDir + normal

randomValue :: IO Double
randomValue = do
  rand <- newStdGen
  let (value, _) = randomR (0.0, 1.0) rand
  return value

randomDirection :: IO Vector3
randomDirection = do
  x <- randomValueNormal
  y <- randomValueNormal
  z <- randomValueNormal
  return $ normalize (Vector3 x y z)

randomValueNormal :: IO Double
randomValueNormal = do
  value <- randomValue
  value' <- randomValue
  return $ sqrt (-2.0 * log value) * cos (2.0 * pi * value')

specularDirection :: Vector3 -> Vector3 -> Vector3
specularDirection normal direction = normalize $ direction - ((normal .* 2) .* (normal `dot` direction))