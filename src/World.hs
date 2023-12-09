module World (World (..), initialWorld) where

import Camera (Camera, initCamera)
import Color
import Light
import Material
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
initSpheres =
  [ Sphere (Vector3 0 0 32) 16 (Material (Color 0 0 0) (Color 1 1 1) Diffuse), -- lightSource
    Sphere (Vector3 2 0 1) 1 (Material (Color 1 0 0) (Color 0 0 0) Diffuse),
    Sphere (Vector3 2 (-2) 0) 1 (Material (Color 1 1 1) (Color 0 0 0) Specular),
    Sphere (Vector3 0 0 (-50)) 50 (Material (Color 0 0 1) (Color 0 0 0) Diffuse)
  ]

initLights :: [Light]
initLights = [Light 4 (Vector3 (-8) 0 8) (Color 1 1 1)]