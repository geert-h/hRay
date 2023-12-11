module Camera (Camera (..), initCamera, aspectRatio, upDirection, rightDirection, screenCentre, leftTop, leftBottom, rightTop) where

import Vector3

data Camera = Camera
  { position :: Vector3,
    direction :: Vector3,
    focalDistance :: Double,
    screenDimensions :: (Int, Int)
  }

initCamera :: Camera
initCamera = Camera (Vector3 (-3) 0 1) (Vector3 1 0 0) 1 (1280, 720)

aspectRatio :: Camera -> Double
aspectRatio cam = fromIntegral (fst $ screenDimensions cam) / fromIntegral (snd $ screenDimensions cam)

upDirection :: Camera -> Vector3
upDirection _ = Vector3 0 0 1

rightDirection :: Camera -> Vector3
rightDirection cam = cross (direction cam) (upDirection cam)

screenCentre :: Camera -> Vector3
screenCentre cam = position cam + direction cam `mulByScalar` focalDistance cam

leftTop :: Camera -> Vector3
leftTop cam = screenCentre cam + upDirection cam - rightDirection cam `mulByScalar` aspectRatio cam

leftBottom :: Camera -> Vector3
leftBottom cam = screenCentre cam - upDirection cam - rightDirection cam `mulByScalar` aspectRatio cam

rightTop :: Camera -> Vector3
rightTop cam = screenCentre cam + upDirection cam + rightDirection cam `mulByScalar` aspectRatio cam