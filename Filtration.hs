module Filtration where

import           Control.Monad
import           Data.Default
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.List as List

import Complex

-- A barcode (contain all the k barcodes merged together)
data Barcode time = Barcode [(Int, (time, time))] deriving (Eq, Ord, Show)

addLine :: Int -> (time, time) -> Barcode time -> Barcode time
addLine k s (Barcode xs) = Barcode $ (k, s) : xs

instance Default (Barcode time) where
  def = Barcode []

-- A filtration
data Filtration time set = Filtration {filtrationList :: [(time, Complex set)]} deriving (Eq, Ord)

instance (Show set, Show field) => Show (Filtration field set) where
  show = foldl (++) "" . map show . filtrationList

-- Transform a simplex into it's normal form simplex, forgeting the orientation
normaliseSimplex :: (Ord set) => Simplex set -> Simplex set
normaliseSimplex a = fst . reorderSimplex $ (a, 0 :: Integer)

-- Because we allow not increasing filtrations, we take the union
setFromFiltration :: (Ord set) => Filtration time set -> Set.Set (Simplex set)
setFromFiltration = List.foldl' Set.union Set.empty . map (Set.map normaliseSimplex . complexSet . snd) . filtrationList
-- setFromFiltration = Set.fromList . map fst . Map.toList . mapFromFiltration


mapFromFiltration :: (Ord set) => Filtration time set -> Map.Map (Simplex set) time
mapFromFiltration = List.foldl' aux Map.empty . join . map complexToSimplices . filtrationList
  where
    complexToSimplices (v, Complex xs) = map (\a -> (v, a)) (Set.toList xs)
    aux :: (Ord set) => Map.Map (Simplex set) field -> (field, Simplex set) -> Map.Map (Simplex set) field
    aux dt (v, s) = Map.insertWith (\_ oldValue -> oldValue) (normaliseSimplex s) v dt


-- Examples

quickFiltration :: [[String]] -> Filtration Rational Char
quickFiltration = Filtration . zip [1..] . map Complex . scanl1 (Set.union) . map (complexSet . quickComplex)

-- Exemple :
--  quickFiltration [["a", "b", "d"], ["ab", "ac"], ["ad", "abc"]]

-- Exemple :
--  let f = quickFiltration [["a", "b", "d"], ["ab", "ca"], ["ad", "abc"]]
--  mapFromFiltration f
