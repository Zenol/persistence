module PointsToFiltration where

import qualified Data.Set.Monad as Set
import           Control.Applicative
import           Control.Monad

import Filtration
import Complex
import Tools

type SimplexSet = Set.Set (Simplex Point)

ripsFiltration :: Distance -> [Point] -> Filtration Double Point
ripsFiltration = Filtration .^ fmap (\(a, b) -> (a, simplicesToComplex b)) .^ ripsPrefiltration

-- For each simplex, we have to add faces, and faces of faces, adn so
simplicesToComplex :: SimplexSet ->  Complex Point
simplicesToComplex simplices = Complex $ (Set.fromList . faces) =<< simplices

-- Produce a list of points in balls of diameters in the range (0, maxDist)  --
ripsPrefiltration :: Distance -> [Point] -> [(Double, SimplexSet)]
ripsPrefiltration d xs = trackEvolv . zip [0 .. maxDist] $ map (ripsComplexStep d) [0 .. maxDist] <*> [xs]
    where
      maxDist :: Double
      maxDist = (*2) $  maximum . map (dInfty (0, 0)) $ xs
      -- Merge steps where the filtration remain constant
      trackEvolv :: [(t, SimplexSet)] -> [(t, SimplexSet)]
      trackEvolv (a@(_,k):b@(_,l):ys) = if k == l
                                        then a : b : (trackEvolv ys)
                                        else a : (trackEvolv ys)
      trackEvolv l = l

-- Compute simplicies made of all points at distance less than dist using the distFct metric.
ripsComplexStep :: Distance -> Double -> [Point] -> SimplexSet
ripsComplexStep distFct dist points = Set.map (Simplex . Set.toList . flip findNeighbour pointSet) pointSet
    where
      pointSet = Set.fromList points
      findNeighbour :: Point -> Set.Set Point -> Set.Set Point
      findNeighbour p = Set.filter (\q -> (distFct p q) <= dist)

-- Metric used to compute filtration. Choose the one you love or build your own.

dInfty :: Distance
dInfty (a, b) (c, d) = fromIntegral $ max (abs (a-c)) (abs (b-d))

dEuclidean :: Distance
dEuclidean (a, b) (c, d) = sqrt . fromIntegral $ (a-c)^2 + (b-d)^2


