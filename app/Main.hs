module Main (main) where

import Camera
import Codec.Picture (DynamicImage (ImageRGB8), PixelRGB8, generateImage, savePngImage)
import Color (Color (Color), colorToRGB8)
import Data.Foldable (foldrM)
import Linearisible (Linearisible (..))
import Ray (Ray (Ray), trace)
import World (World (..), initialWorld)

main :: IO ()
main = do
  worldToImage initialWorld

worldToImage :: World -> IO ()
worldToImage world = do
  let cam = camera world
  let rayCount = 50
  let (imgWidth, imgHeight) = screenDimensions cam
  pixels <-
    sequence
      [sequence [handlePixel rayCount world x y | x <- [0 .. imgWidth - 1]] | y <- [0 .. imgHeight - 1]]

  let image = uncurry (generateImage (\x y -> pixels !! y !! x)) (screenDimensions (camera world))
  savePngImage "picture.png" (ImageRGB8 image)

handlePixel :: Int -> World -> Int -> Int -> IO PixelRGB8
handlePixel rayCount world@(World cam@(Camera pos _ _ (w, h)) _ _) x y = do
  outputColor <-
    foldrM
      ( \_ acc -> do
          it <- getIter
          return $ it + acc
      )
      (Color 0 0 0)
      [0 .. rayCount - 1]
  pure $ colorToRGB8 $ outputColor ./ fromIntegral rayCount
  where
    a = fromIntegral x / fromIntegral w
    b = fromIntegral y / fromIntegral h
    u = rightTop cam - leftTop cam
    v = leftBottom cam - leftTop cam
    screenPointer = leftTop cam + u .* a + v .* b
    direction = screenPointer - position cam
    ray = Ray pos direction
    getIter = do trace 0 world ray
