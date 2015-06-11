module Tools where

(.^) :: (c -> d) -> (a -> b -> c) -> (a -> b -> d)
(.^) = (.) . (.)
infixr 9 .^

type Point = (Int, Int)
type Distance = (Point -> Point -> Double)

