module PointsToFiltration where

import qualified Data.Set as Set
import           Control.Applicative

import Filtration
import Complex

type Point = (Int, Int)


--- WIP. Not not use that  --
ripsFiltration :: [(Int, Int)] -> Filtration Int (Int, Int)
ripsFiltration xs = Filtration [(0, complex)]
    where
      complex = foldl nextComplex (Complex Set.empty) $ (map (ripsComplexStep dInfty) [0 .. maxDist]) <*> [xs]
      nextComplex :: Complex Point -> [Simplex Point] -> Complex Point
      nextComplex complex simplicies =  Complex $ Set.union (complexSet complex) (Set.fromList simplicies)
      maxDist :: Int
      maxDist = (*2) $ maximum . map (\(a, b) -> a + b) $ xs

-- Compute simplicies made of all points at distance less than dist using the distFct metric.
ripsComplexStep :: (Point -> Point -> Int) -> Int -> [Point] -> [Simplex Point]
ripsComplexStep distFct dist points = map (Simplex . flip findNeighbour points) points
    where
      findNeighbour :: Point -> [Point] -> [Point]
      findNeighbour p = filter (\q -> (distFct p q) < dist)

dInfty :: Point -> Point -> Int
dInfty (a, b) (c, d) = max (abs (a-c)) (abs (b-d))
