module World (World (..), initialWorld) where

import Camera (Camera, initCamera)
import Color
import Light
import Material
import Sphere
import Vector3

data World = World
  { camera :: !Camera,
    spheres :: ![Sphere],
    lights :: ![Light]
  }

initialWorld :: World
initialWorld = World initCamera initSpheres initLights

initSpheres :: [Sphere]
initSpheres =
  [ Sphere (Vector3 0 0 36) 24 (Material (Color 0 0 0) (Color 1 1 1) Normal), -- lightSource
    Sphere (Vector3 2 0 1) 1 (Material (Color 1 1 1) (Color 0 0 0) Normal),
    Sphere (Vector3 2 (-3) 1) 1 (Material (Color 1 0 0) (Color 0 0 0) Normal),
    Sphere (Vector3 0 0 (-50)) 50 (Material (Color 0 0 1) (Color 0 0 0) Normal)
  ]

initSpheres' :: [Sphere]
initSpheres' =
  [ Sphere (Vector3 (1e5 + 1) 40.8 81.6) 1e5 (Material (Color 0.75 0.25 0.25) 0 Diffuse),
    Sphere (Vector3 (-1e5 + 99) 40.8 81.6) 1e5 (Material (Color 0.25 0.25 0.75) 0 Diffuse),
    Sphere (Vector3 50 40.8 1e5) 1e5 (Material (Color 0.75 0.75 0.25) 0 Diffuse),
    Sphere (Vector3 50 40.8 (-1e5 + 170)) 1e5 (Material 0 0 Diffuse),
    Sphere (Vector3 50 1e5 81.6) 1e5 (Material (Color 0.75 0.75 0.25) 0 Diffuse),
    Sphere (Vector3 50 (-1e5 + 81.6) 81.6) 1e5 (Material (Color 0.75 0.75 0.75) 0 Diffuse),
    Sphere (Vector3 27 16.5 47) 16.5 (Material (Color 0.999 0.999 0.999) 0 Specular),
    -- Sphere (Vector3 73 16.5 78) 16.5 (Material (Color 0.999 0.999 0.999) 0 Refractive),
    Sphere (Vector3 50 (681.6 - 0.27) 81.6) 600 (Material (Color 12 12 12) 0 Diffuse)
  ]

initLights :: [Light]
initLights = [Light 4 (Vector3 (-8) 0 8) (Color 1 1 1)]