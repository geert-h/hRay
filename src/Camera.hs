module Camera (Camera (..), aspectRatio, upDirection, rightDirection, screenCentre, leftTop, leftBottom, rightTop) where

import Linearisible (Linearisible (..))
import Vector3

data Camera = Camera
  { position :: Vector3,
    direction :: Vector3,
    focalDistance :: Double,
    screenDimensions :: (Int, Int)
  }

aspectRatio :: Camera -> Double
aspectRatio cam = fromIntegral (fst $ screenDimensions cam) / fromIntegral (snd $ screenDimensions cam)

upDirection :: Camera -> Vector3
upDirection _ = Vector3 0 0 1

rightDirection :: Camera -> Vector3
rightDirection cam = cross (direction cam) (upDirection cam)

screenCentre :: Camera -> Vector3
screenCentre cam = position cam + direction cam .* focalDistance cam

leftTop :: Camera -> Vector3
leftTop cam = screenCentre cam + upDirection cam - rightDirection cam .* aspectRatio cam

leftBottom :: Camera -> Vector3
leftBottom cam = screenCentre cam - upDirection cam - rightDirection cam .* aspectRatio cam

rightTop :: Camera -> Vector3
rightTop cam = screenCentre cam + upDirection cam + rightDirection cam .* aspectRatio cam