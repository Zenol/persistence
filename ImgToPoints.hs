module ImgToPoints where

import Codec.Picture
import Codec.Picture.Types
import Data.Word

{--
    Load an RGB umage, select pixels far enought from white, and return a list of coordinate (x, y) of those points.

    You can test it using a picture "pict.png" with the following code :

    main = do
      points <- loadPoints  "pict.png"
      case points of
        Left errmsg -> print errmsg >> return []
        Right xs -> return xs

 --}
loadPoints :: String -> IO (Either String [(Int, Int)])
loadPoints fileName = fmap (>>= imageToPoints) (readImage fileName)

imageToPoints :: DynamicImage -> Either String [(Int, Int)]
imageToPoints (ImageRGB8 image@(Image w h _)) = Right $ pixelFold (pixel2pointFolder' 550) [] image 
imageToPoints (ImageRGBA8 image@(Image w h _)) = Right $ pixelFold (pixel2pointFolder 550) [] image 
imageToPoints _ = Left "Invalid pixel format."

pixel2pointFolder' :: Int -> [(Int, Int)] -> Int -> Int -> PixelRGB8 -> [(Int, Int)]
pixel2pointFolder' treshold acc x y pixel = pixel2pointFolder treshold acc x y (promotePixel pixel)

pixel2pointFolder :: Int -> [(Int, Int)] -> Int -> Int -> PixelRGBA8 -> [(Int, Int)]
pixel2pointFolder treshold acc x y (PixelRGBA8 a b c _) = if (fromIntegral a + fromIntegral b + fromIntegral c) < treshold
    then (x, y) : acc
    else acc

isMiddlePixelRed :: FilePath -> IO (Maybe Bool)
isMiddlePixelRed fp = do
    image <- readImage fp
    case image of
        Left _ -> return Nothing
        Right image' -> return (go image')
  where
    go :: DynamicImage -> Maybe Bool
    go (ImageRGB8 image@(Image w h _)) =
        Just (isRed (pixelAt image (w `div` 2) (h `div` 2)))
    go _ = Nothing
    isRed :: PixelRGB8 -> Bool
    isRed (PixelRGB8 r g b) = r == maxBound && g == 0 && b == 0
