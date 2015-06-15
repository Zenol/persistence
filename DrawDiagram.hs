{-# LANGUAGE NoMonomorphismRestriction #-}
module Main where

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
import Data.Monoid
import Control.Applicative
import System.Environment
import qualified Data.Set.Monad as Set

import Filtration
import Persistence
import ImgToPoints
import PointsToFiltration
import Complex
import qualified Tools

-- Compute star size from barcode values
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
      -- Compute barcode and keep only 1 and 0 homology
      gBarcode = Barcode . filter (\(a,_) -> a <= 1) $ xs
      Barcode xs = computeBarcode (0 :: Double) filt

borderMax :: Barcode Double -> Double
borderMax (Barcode xs) = maximum . map snd . map snd $ xs

border :: Double -> Diagram B
border s = hrule ((sqrt 2) * s) # translate (r2 ((sqrt 2) * s/2, 0)) # rotateBy (1/8) 
           `atop` hrule s # translate (r2 (s/2, s))
           `atop` vrule s # translate (r2 (0, s/2))

mapSnd :: (b -> c) -> (a, b) -> (a, c)
mapSnd f (a, b) = (a, f b)
-- Keep only 3-simplices, since the n-homology is null for n >= 3
simp2D :: Filtration Double Tools.Point -> Filtration Double Tools.Point
simp2D = Filtration . trackEvolv . map (mapSnd filterComplex) . filtrationList
    where
      filterComplex = Complex . Set.filter ((<= 3) . length . simplexList) . complexSet

-- Draw a persistence diagram from a barcode from an Image
-- We add simp2D into the filtering pipe in order to compute only the 0 and 1 homology.
main :: IO Int
main = do
   putStrLn "Input file name (.bmp, .png, ...):"
   input <- getLine
   points <- loadPoints input
   case points of
       Left str -> putStrLn str >> return 42
       Right pts -> mainWith (diag $ dt)
--                    >> (putStrLn . show $ dt)
--                    >> (putStrLn . show $ computeBarcode (0 :: Double) dt)
                    >> return 0
                    where
                      dt = simp2D .  ripsFiltration dEuclidean $ pts

