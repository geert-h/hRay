module World (World (..), initialWorld) where

import Camera (Camera, initCamera)
import Color
import Light
import Material (Material (Material))
import Sphere
import Vector3

data World = World
  { camera :: Camera,
    spheres :: [Sphere],
    lights :: [Light]
  }

initialWorld :: World
initialWorld = World initCamera initSpheres initLights

initSpheres :: [Sphere]
initSpheres = [Sphere (Vector3 2 0 1) 1 (Material (Color 1 0 0) (Color 0 0 0) 0 0), Sphere (Vector3 2 0 (-2)) 1 (Material (Color 1 1 0) (Color 0 0 0) 0 0)]

initLights :: [Light]
initLights = [Light 4 (Vector3 (-8) 0 8) (Color 1 1 1)]