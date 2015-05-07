{-# LANGUAGE NoMonomorphismRestriction #-}

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
import Data.Monoid
import Control.Applicative

import Filtration
import Persistence

-- Global settings
gStarSize = borderMax / 20
f = quickFiltration [ ["a", "b"]
                    , ["c", "d", "ab", "bc"]
                    , ["cd", "ad"]
                    , ["ac"]
                    , ["abc"]
                    , ["acd"]
                    , ["adb", "dcb"]
                    ]
gBarcode = computeBarcode (0 :: Double) f

-- Output diagrams

colors :: (Floating a, Ord a) => [Colour a]
colors = cycle [red, blue, green, yellow, pink, olive, cyan, orange, grey, purple]

cross :: Double -> Colour Double -> Diagram B
cross sz color = star (StarSkip 3) (regPoly 7 sz)
                   # strokeP
                   # fcA (color `withOpacity` 0.3)
                   # lc color

drawBarcode :: Barcode Double -> Diagram B
drawBarcode (Barcode xs) = foldl1 (atop) $ zipWith drawCross la lb
  where
    (la, lb) = unzip xs
    drawCross colorIdx coordinates = cross gStarSize (colors!!colorIdx) # (translate . r2 $ coordinates)


diag :: Diagram B
diag = drawBarcode gBarcode `atop` (border borderMax)

borderMax :: Double
borderMax = maximum . map snd . map snd $ xs
  where Barcode xs = gBarcode

border :: Double -> Diagram B
border s = hrule ((sqrt 2) * s) # translate (r2 ((sqrt 2) * s/2, 0)) # rotateBy (1/8) 
           `atop` hrule s # translate (r2 (s/2, s))
           `atop` vrule s # translate (r2 (0, s/2))

-- Draw a persistence diagram from a barcode
main :: IO ()
main = mainWith diag
