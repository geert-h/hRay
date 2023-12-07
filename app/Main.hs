module Main (main) where

import Camera
import Codec.Picture (DynamicImage (ImageRGB8), PixelRGB8, generateImage, savePngImage)
import Color (Color (Color), colorToRGB8)
import Ray (Ray (Ray), trace)
import Vector3 (mulByScalar)
import World (World (..), initialWorld)

main :: IO ()
main = do
  worldToImage initialWorld

worldToImage :: World -> IO ()
worldToImage world = do
  let image = uncurry (generateImage (handlePixel world)) (screenDimensions (camera world))
  savePngImage "picture.png" (ImageRGB8 image)

genPixelFromXandYValue :: World -> Int -> Int -> PixelRGB8
genPixelFromXandYValue _ x y = colorToRGB8 $ Color (fromIntegral x / 1920) (fromIntegral y / 1080) 0

handlePixel :: World -> Int -> Int -> IO PixelRGB8
handlePixel world@(World cam@(Camera pos _ _ (w, h)) _ _) x y = colorToRGB8 outputColor
  where
    a = fromIntegral x / fromIntegral w
    b = fromIntegral y / fromIntegral h

    u = rightTop cam - leftTop cam
    v = leftBottom cam - leftTop cam

    screenPointer = leftTop cam + u `mulByScalar` a + v `mulByScalar` b
    direction = screenPointer - position cam

    ray = Ray pos direction
    outputColor = trace 30 (Color 0 0 0) world ray
