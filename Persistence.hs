module Persistence where

import           Control.Monad
import           Data.Default
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.Permute as Permute
import qualified Data.List as List
import           Debug.Trace (trace)

import Complex
import Filtration

data AlgoState time field set = AlgoState
    { table      :: Map.Map (Simplex set) (Simplex set, Chain field set)
    , barcode    :: Barcode time
    , markedList :: [Simplex set]
    } deriving (Eq, Ord, Show)

-- Think about re,oving all those boilerplate
addToTable :: Ord set =>Simplex set -> (Simplex set, Chain field set) -> AlgoState time field set -> AlgoState time field set
addToTable idx value st = st {table = Map.insert idx value (table st)}
updateBarcode :: (Num field) => Int -> (time, time) -> AlgoState time field set -> AlgoState time field set
updateBarcode k v st = st {barcode = addBar k v (barcode st)}
markSimplex :: (Num field) => Simplex set -> AlgoState time field set -> AlgoState time field set
markSimplex simplex st = st { markedList = simplex : (markedList st) }

instance Default (AlgoState time field set) where
  def = AlgoState Map.empty def []

computeBarcode :: (Ord set, Eq field, Fractional field, Num field, Num time, Show field, Show set) => field -> Filtration time set -> Barcode time
computeBarcode vvv filtration = barcode . storeInfiniteSegments . foldl updateWithSimplex def $ simplices
  where
    -- Data
    degrees = mapFromFiltration filtration
    simplices = Set.toList . setFromFiltration $ filtration
    getDegree simplex = Map.findWithDefault 0 simplex degrees
    maxIndex = fst . Map.findMax . chainMap
    infinity = snd . Map.findMax $ degrees

    --Heart of the algorithm
--  updateWithSimplex :: (Ord set) => AlgoState time field set -> Simplex set -> AlgoState time field set
    updateWithSimplex st simplex = if (d == zero)
      then
        markSimplex simplex st
      else
        addToTable simplex' (simplex, d) $
        updateBarcode (dim simplex') (getDegree simplex', getDegree simplex) st
      where
        d = removePivotRow (deltaSimplex simplex) st
        _ = (vvv) .* d -- ugly but infer types
        simplex' = maxIndex d

    removePivotRow d st = simplify d' st
      where
        d' = Chain $ Map.intersection (chainMap d) (Map.fromList $ List.zip (markedList st) [1 :: Integer ..])
        simplify d st = if d == zero
          then d
          else case Map.lookup simplex' (table st) of
                 Nothing         -> d
                 Just (_, chain) -> trace (show (d, simplex', chain)) $ let q = (Map.findWithDefault 0 simplex' . chainMap $ chain)
                                                                            a = (Map.findWithDefault 0 simplex' . chainMap $ d)
                                    in simplify (d .- ((a/q) .* chain)) st -- In the case of the quotient isn't exact; we would like to set the simplex' coef of d to zero)
         where
          simplex' = maxIndex d
           

    -- Finish with adding infinite components to barcode
--  storeInfiniteSegments :: (Ord set) => AlgoState time field set -> AlgoState time field set
    storeInfiniteSegments st = foldl checkSimplex st (markedList st   )
      where
        checkSimplex state simplex = case Map.lookup simplex (table state) of
          Nothing -> updateBarcode (dim simplex) (getDegree simplex, infinity) state
          Just _  -> state

-- Example
-- let f = quickFiltration [["a", "b", "c"], ["ab", "ac"], ["abc"]]
-- let f = quickFiltration [["a", "b", "c"], ["ab", "ac"], ["abc"]]
-- computeBarcode (0 :: Rational) f
