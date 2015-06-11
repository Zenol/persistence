{-# LANGUAGE NoMonomorphismRestriction #-}

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
import Data.Monoid
import Control.Applicative
import System.Environment

import Filtration
import Persistence
import ImgToPoints
import PointsToFiltration
import qualified Tools

-- Global settings
gBarcode points = computeBarcode (0 :: Double) points
gStarSize gBarcode = borderMax gBarcode / 20

-- Output diagrams

colors :: (Floating a, Ord a) => [Colour a]
colors = cycle [red, blue, green, yellow, pink, olive, cyan, orange, grey, purple]

cross :: Double -> Colour Double -> Diagram B
cross sz color = star (StarSkip 3) (regPoly 7 sz)
                   # strokeP
                   # fcA (color `withOpacity` 0.3)
                   # lc color

drawBarcode :: Barcode Double -> Diagram B
drawBarcode gBarcode@(Barcode xs) = foldl1 (atop) $ zipWith drawCross la lb
  where
    (la, lb) = unzip xs
    drawCross colorIdx coordinates = cross (gStarSize gBarcode) (colors!!colorIdx) # (translate . r2 $ coordinates)


diag :: Filtration Double Tools.Point -> Diagram B
diag filt = drawBarcode gBarcode `atop` (border $ borderMax gBarcode)
    where
      gBarcode = computeBarcode (0 :: Double) filt

borderMax :: Barcode Double -> Double
borderMax (Barcode xs) = maximum . map snd . map snd $ xs

border :: Double -> Diagram B
border s = hrule ((sqrt 2) * s) # translate (r2 ((sqrt 2) * s/2, 0)) # rotateBy (1/8) 
           `atop` hrule s # translate (r2 (s/2, s))
           `atop` vrule s # translate (r2 (0, s/2))

-- Draw a persistence diagram from a barcode
main :: IO Int
main = do
   putStrLn "Input file name (.bmp, .png, ...):"
--   input <- getLine
   points <- loadPoints "./pict1.bmp"
   case points of
       Left str -> putStrLn str >> return (-2)
       Right pts -> mainWith (diag . ripsFiltration dEuclidean $ pts) >> return 0
   return 0
