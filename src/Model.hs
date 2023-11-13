module Model where

data State = State
  { screenWidth :: Int,
    screenHeight :: Int

  }

data Camera = Camera
  { position :: (Float, Float, Float)
  }

