module Persistence where

import           Control.Monad
import           Data.Default
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.Permute as Permute
import qualified Data.List as List

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
updateBarcode k v st = st {barcode = addLine k v (barcode st)}
markSimplex :: (Num field) => Simplex set -> AlgoState time field set -> AlgoState time field set
markSimplex simplex st = st { markedList = simplex : (markedList st) }

instance Default (AlgoState time field set) where
  def = AlgoState Map.empty def []

computeBarcode :: (Ord set, Eq field, Num field, Num time) => field -> Filtration time set -> Barcode time
computeBarcode vvv filtration = barcode . storeInfiniteSegments . foldl updateWithSimplex def $ simplices
  where
    -- Data
    degrees = mapFromFiltration filtration
    simplices = Set.toList . setFromFiltration $ filtration
    getDegree simplex = Map.findWithDefault 0 simplex degrees

    --Heart of the algorithm
--  updateWithSimplex :: (Ord set) => AlgoState time field set -> Simplex set -> AlgoState time field set
    updateWithSimplex st simplex = if (d == zero)
      then
        markSimplex simplex st
      else
        addToTable simplex' (simplex, d) $
        updateBarcode (dim simplex') (getDegree simplex', getDegree simplex) st'
      where
        (d, st') = removePivotRow (deltaSimplex simplex) st
        _ = (vvv) .* d -- ugly but infer types
        simplex' = fst . Map.findMax . chainMap $ d

    removePivotRow d st = (d', st)
      where
        d' = Chain $ Map.intersection (chainMap d) (Map.fromList $ List.zip (markedList st) [1..])

    -- Finish with adding infinite components to barcode
--  storeInfiniteSegments :: (Ord set) => AlgoState time field set -> AlgoState time field set
    storeInfiniteSegments st = foldl checkSimplex st (markedList st   )
      where
        checkSimplex state simplex = case Map.lookup simplex (table state) of
          Nothing -> updateBarcode (dim simplex) (getDegree simplex, 666) state
          Just _  -> state

-- Example
-- let f = quickFiltration [["a", "b", "c"], ["ab", "ac"], ["abc"]]
-- computeBarcode (0 :: Rational) f
