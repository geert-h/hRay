module Model (World (..), Camera (..), initialWorld) where

data World = World
  { screenWidth :: Int,
    screenHeight :: Int
  }

data Camera = Camera
  { position :: (Float, Float, Float)
  }

initialWorld :: World
initialWorld = World 1920 1080