module Main (main) where

import Codec.Picture (DynamicImage (ImageRGB8), PixelRGB8 (PixelRGB8), generateImage, savePngImage)
import Model (World (..), initialWorld)

main :: IO ()
main = do
  worldToImage initialWorld

worldToImage :: World -> IO ()
worldToImage world = do
  let image = generateImage genPixelFromXandYValue (screenWidth world) (screenHeight world)
  savePngImage "picture.png" (ImageRGB8 image)

genPixelFromXandYValue :: Int -> Int -> PixelRGB8
genPixelFromXandYValue x y = PixelRGB8 (fromIntegral $ x `mod` 255) (fromIntegral $ y `mod` 255) (fromIntegral $ x `mod` 255)