module Complex where

import           Control.Monad
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.Permute as Permute
import qualified Data.List as List

-- A complex
data Complex set = Complex {complexSet :: (Set.Set (Simplex set))} deriving (Eq, Ord)

instance Show set => Show (Complex set) where
  show = show . Set.toList . complexSet

-- A simplex
data Simplex set = Simplex {simplexList :: [set]} deriving (Eq)

-- Dimension of a simplex
dim :: Simplex set -> Int
dim = (+) (-1) . length . simplexList

-- Order on simplices. It's not the same as order on list.
instance Ord set => Ord (Simplex set) where
  compare (Simplex a) (Simplex b) =
      case () of _
                    | la > lb -> GT
                    | la < lb -> LT
                    | otherwise -> compare a b
    where
      la = length a
      lb = length b

instance Show set => Show (Simplex set) where
  show (Simplex v) = show v

-- A chain complex
data Chain field set = Chain {chainMap ::  (Map.Map (Simplex set) field)} deriving (Eq, Ord)

instance (Show set, Show field) => Show (Chain field set) where
  show (Chain dt) = "Chain " ++ show (Map.toList dt)


-- Re-order simplices
reorderSimplex :: (Num field, Ord set) => (Simplex set, field) -> (Simplex set, field)
reorderSimplex (Simplex xs, v) = (Simplex ys, s * v)
    where
      s = if (Permute.isEven perm) then 1 else -1
      (ys, perm) = Permute.sort (length xs) xs

-- Re-order simplices of a chain
reorder :: (Ord set, Num field, Eq field) => Chain field set -> Chain field set
reorder = List.foldl' (.+) zero . map (Chain . Map.fromList . return) . (map reorderSimplex) . Map.toList . chainMap

-- Remove 0 value from a chain complex
cleanChain :: (Eq field, Num field) => Chain field set -> Chain field set
cleanChain = Chain . (Map.filter (\a -> a /= 0)) . chainMap

-- Reduce a chain to a normal form
reduce :: (Ord set, Num field, Eq field) => Chain field set -> Chain field set
reduce = cleanChain . reorder

-- Sum two chain
(.+) :: (Ord set, Num field, Eq field) => (Chain field set) -> (Chain field set) -> (Chain field set)
(.+) (Chain cl) (Chain cr) = cleanChain $ Chain (Map.unionWith (\a b -> a + b) cl cr)

zero :: Chain set field
zero = Chain Map.empty

-- Negate two chain
neg :: (Ord set, Num field) => (Chain field set) -> (Chain field set)
neg (Chain cl) = Chain (Map.map (\a -> -a) cl)

-- Substract two chain
(.-) :: (Ord set, Num field, Eq field) => (Chain field set) -> (Chain field set) -> (Chain field set)
(.-) a b = a .+ (neg b)

(.*) :: (Num field, Eq field) => field -> (Chain field set) -> (Chain field set)
(.*) 0 _ = zero
(.*) scalar (Chain dt) = Chain $ Map.map ((*) scalar) dt

-- Calculate the boundary of a simplex
deltaSimplex :: (Num field, Ord set) => Simplex set -> Chain field set
deltaSimplex (Simplex [_]) = zero
deltaSimplex (Simplex [])  = zero
deltaSimplex (Simplex xs)  = Chain $ (Map.fromList $ browser 1 [] xs)
  where
    browser _ _ [] = []
    browser s l (v : r) = (Simplex (l ++ r), s) : (browser (-s) (l ++ [v]) r)

-- Calculate the boundary of a chain
deltaChain :: (Num field, Ord set, Eq field) => Chain field set -> Chain field set
deltaChain = Map.foldlWithKey' adder zero . chainMap
  where
    adder :: (Num field, Eq field, Ord set) => Chain field set -> Simplex set -> field -> Chain field set
    adder stack k b = b .* (deltaSimplex k) .+ stack

-- Calculate the boundary of a chain and reduce it to it's normal form
delta :: (Num field, Ord set, Eq field) => Chain field set -> Chain field set
delta = reduce . deltaChain

-- Exemples

quickComplex :: [String] -> Complex Char
quickComplex = Complex . Set.fromList . map Simplex

quickChain :: [String] -> Chain Rational Char
quickChain [] = zero
quickChain (x : xs) = (quickChain xs) .+ (Chain $ Map.fromList [(Simplex x, 1)])

quickChain' :: [(Rational, String)] -> Chain Rational Char
quickChain' [] = zero
quickChain' ((n, x) : xs) = (quickChain' xs) .+ (Chain $ Map.fromList [(Simplex x, n)])

-- Construction d'une chaine ou chaque simplex est d'occurence 1 :
-- quickChain ["ab", "bc", "cd"]

-- Calcul du bord d'un tetrahèdre :
-- delta $ quickChain' [(1, "abd"),(1, "bcd"), (1, "adc"), (-1, "abc")]

-- Construction rapide d'un complexe
-- quickComplex ["abc", "ab", "ac"]
