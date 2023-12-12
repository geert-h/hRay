module Color (Color (..), colorToRGB8, fromDouble) where

import Codec.Picture (PixelRGB8 (PixelRGB8))
import Data.Word (Word8)
import Linearisible (Linearisible (..))

data Color = Color
  { r :: {-# UNPACK #-} !Double,
    g :: {-# UNPACK #-} !Double,
    b :: {-# UNPACK #-} !Double
  }
  deriving (Eq, Show)

instance Num Color where
  (+) v1 v2 = Color (r v1 + r v2) (g v1 + g v2) (b v1 + b v2)
  (-) v1 v2 = Color (r v1 - r v2) (g v1 - g v2) (b v1 - b v2)
  (*) v1 v2 = Color (r v1 * r v2) (g v1 * g v2) (b v1 * b v2)
  abs v = Color (abs $ r v) (abs $ g v) (abs $ b v)
  negate v = Color (-r v) (-g v) (-b v)
  signum v = Color (signum $ r v) (signum $ g v) (signum $ b v)
  fromInteger v = Color (fromInteger v) (fromInteger v) (fromInteger v)

instance Linearisible Color where
  maxL c = max (r c) (max (g c) (b c))
  c ./ s = Color (r c / s) (g c / s) (b c / s)
  c .* s = Color (r c * s) (g c * s) (b c * s)
  lc `dot` rc = r lc * r rc + g lc * g rc + b lc * b rc
  normalize c = c ./ size c
  size c = sqrt $ c `dot` c

fromDouble :: Double -> Color
fromDouble v = Color v v v

colorToRGB8 :: Color -> PixelRGB8
colorToRGB8 clr = PixelRGB8 (doubleColorValueToWord8 $ r clr) (doubleColorValueToWord8 $ g clr) (doubleColorValueToWord8 $ b clr)

doubleColorValueToWord8 :: Double -> Word8
doubleColorValueToWord8 v = max 0 $ min 255 (truncate (v * 255))
