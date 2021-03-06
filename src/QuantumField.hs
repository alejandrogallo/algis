module QuantumField where

import           Prelude                 hiding ( (*)
                                                , (+)
                                                , (**)
                                                )
import           Data.List
import           Data.Monoid

infixl 7 *
infixl 6 +

type Index = String
type Field = Double

data Scalar
  = Delta String String
  | Tensor String Index Index
  | ParticleSign
  | ScalarZero
  | ScalarOne
  | Val Field
  | Conj Field
  | Sprod [Scalar]
  | Ssum [Scalar]
  deriving (Show, Eq)

data Operator
  = FockOne
  | FockZero
  | Op Index
  | Particle Index
  | Hole Index
  | Dagger Operator
  | Oprod [Operator]
  | Osum [Operator]
  | Scaled Scalar Operator
  deriving(Show, Eq)

class CStarALgebra a where
  (*) :: a -> a -> a
  (**) :: a -> Int -> a
  (+) :: a -> a -> a
  (^+) :: a -> a

instance CStarALgebra Operator where
  (^+) = dag

  FockOne   * a         = a
  a         * FockOne   = a
  (Oprod a) * (Oprod b) = Oprod $ a ++ b
  (Oprod a) * b         = Oprod $ a ++ [b]
  b         * (Oprod a) = Oprod $ b : a
  a         * b         = Oprod [a, b]

  a ** n | n <= 0    = Oprod []
         | otherwise = a * (a ** (n - 1))

  b        + FockZero = b
  FockZero + b        = b
  (Osum a) + (Osum b) = Osum $ a ++ b
  (Osum a) + b        = Osum $ a ++ [b]
  b        + (Osum a) = Osum $ b : a
  a        + b        = Osum [a, b]

conj :: Scalar -> Scalar
conj (Delta s t   ) = Delta s t
conj (Val   s     ) = Conj s
conj (Conj  s     ) = Val s
conj (Sprod t     ) = Sprod $ map conj t
conj (Ssum  t     ) = Ssum $ map conj t
conj (Tensor s a i) = Tensor s a i
conj ScalarZero     = ScalarZero
conj ScalarOne      = ScalarOne

dag :: Operator -> Operator
dag (Dagger (Particle i)) = Particle i
dag (Dagger (Hole     a)) = Hole a
dag (Dagger (Op       a)) = Op a
dag (Oprod  a           ) = Oprod $ map dag $ reverse a
dag (Osum   a           ) = Osum $ map dag a
dag (Scaled s o         ) = Scaled (conj s) $ dag o
dag FockOne               = FockOne
dag FockZero              = FockZero
dag o                     = Dagger o

isOneBody :: Operator -> Bool
isOneBody (Op       x           ) = True
isOneBody (Dagger   (Op x)      ) = True
isOneBody (Particle x           ) = True
isOneBody (Dagger   (Particle x)) = True
isOneBody (Hole     x           ) = True
isOneBody (Dagger   (Hole x)    ) = True
isOneBody (Scaled s p           ) = isOneBody p
isOneBody (Osum q               ) = all isOneBody q
isOneBody _                       = False

normalize :: Operator -> Operator
normalize (Osum  s) = foldl (+) FockZero $ map normalize s
normalize (Oprod s) = Oprod (normSort s)
 where
  normSort :: [Operator] -> [Operator]
  normSort (x : xs) = case x of
    (Op       a) -> (normSort xs) ++ [x]
    (Particle a) -> (normSort xs) ++ [x]
    (Hole     a) -> (normSort xs) ++ [x]
    _            -> x : (normSort xs)
  normSort [] = []
normalize (Scaled a b) = Scaled a (normalize b)
normalize a            = a

bubbleSortIteration :: (Ord a) => ([a], Int) -> ([a], Int)
bubbleSortIteration ([x], n) = ([x], n)
bubbleSortIteration ((x : y : xs), n)
  | (x > y)
  = let (ys, n') = (bubbleSortIteration (x : xs, Prelude.sum [n, 1]))
    in  (y : ys, n')
  | otherwise
  = let (ys, n') = (bubbleSortIteration (y : xs, n)) in (x : ys, n')

bubbleSortCount :: (Ord a) => ([a], Int, Int) -> ([a], Int, Int)
bubbleSortCount (xs, n, i) = if (length xs == i)
  then (xs, n, i)
  else bubbleSortCount (ys, m, sum [i, 1])
  where (ys, m) = bubbleSortIteration (xs, n)

bubbleSortWithTranspositions :: (Ord a) => [a] -> ([a], Int)
bubbleSortWithTranspositions xs = (ys, transpositinoNumber)
  where (ys, transpositinoNumber, _) = bubbleSortCount (xs, 0, 0)

permutationSign :: (Ord a) => [a] -> Int
permutationSign xs = sign
 where
  (ys, transpositinoNumber, _) = bubbleSortCount (xs, 0, 0)
  sign                         = (-1) ^ transpositinoNumber

type WickPair = [(Operator, Int)]

-- This function should return a list of a way of pairing
-- operators in an expression, two by two
contractedCombinations :: [Operator] -> [[WickPair]]
contractedCombinations s = seqs
 where
  seqs = map getPairs $ filter ((== 0) . (`mod` 2) . length) $ subsequences
    (zip s [1 ..])
  getPairs :: [a] -> [[a]]
  getPairs = filter ((== 2) . length) . subsequences

combinePairingsWithProduct
  :: [[WickPair]] -> [Operator] -> [[Operator]]
combinePairingsWithProduct cs ops = (map filterOutContraction cs) <*> [ops]
 where
  filterOutContraction :: [WickPair] -> [Operator] -> [Operator]
  filterOutContraction c ops =
    [ o | o <- ops, not (o `elem` (map fst $ mconcat c)) ]

-- This should return a sum of wicked operators
{-
wick :: Operator -> Operator
wick (Osum  s) = foldl (+) FockZero $ map wick s
wick (Oprod s) = foldl (+) FockZero $ map (normalize . Oprod) wickedTerms
  where wickedTerms = pairAll pairings s
        pairings = contractedCombinations s
        pairAll :: [[[(Operator, Int)]]] -> [Operator] -> [[Operator]]
        pairAll combs ops = map
-}

expand :: Operator -> Operator
expand (Oprod c   ) = Osum $ map Oprod (expandList c)
expand (Osum  a   ) = foldl (+) FockZero (map (expand . Oprod . (: [])) a)
expand (Scaled s a) = Osum [Scaled s (expand a)]
expand c            = Osum [c]

expandList :: [Operator] -> [[Operator]]
expandList l@(x : xs) = case x of
  (Oprod a) -> expandList $ a ++ xs
  (Osum  a) -> mconcat $ map expandList $ [(:)] <*> a <*> [xs]
  _         -> map (x :) (expandList xs)
expandList [] = [[]]

contractRealVacuum :: Operator -> Operator -> Scalar
-- op - hole
contractRealVacuum (Op       x           ) (Dagger   (Op y)      ) = Delta x y
contractRealVacuum (Dagger   (Op y)      ) (Op       x           ) = ScalarZero
contractRealVacuum (Op       x           ) (Op       y           ) = ScalarZero
-- hole - hole
contractRealVacuum (Hole     x           ) (Dagger   (Hole y)    ) = Delta x y
contractRealVacuum (Dagger   (Hole y)    ) (Hole     x           ) = ScalarZero
contractRealVacuum (Hole     x           ) (Hole     y           ) = ScalarZero
-- particle - particle
contractRealVacuum (Particle x           ) (Dagger   (Particle y)) = Delta x y
contractRealVacuum (Dagger   (Particle y)) (Particle x           ) = ScalarZero
contractRealVacuum (Particle x           ) (Particle y           ) = ScalarZero
-- particle - hole
contractRealVacuum (Particle x           ) (Hole     y           ) = ScalarZero
contractRealVacuum (Dagger   (Hole x)    ) (Particle y           ) = ScalarZero
contractRealVacuum (Hole     x           ) (Dagger   (Particle y)) = ScalarZero
-- The whole system should panic here
contractRealVacuum _                       _                       = undefined

contractSDVacuum :: Operator -> Operator -> Scalar
-- hole - hole
contractSDVacuum (Dagger   (Hole x)) (Hole     y           ) = Delta x y
contractSDVacuum (Hole     x       ) (Hole     y           ) = ScalarZero
-- particle - particle
contractSDVacuum (Particle x       ) (Dagger   (Particle y)) = Delta x y
contractSDVacuum (Particle x       ) (Particle y           ) = ScalarZero
-- particle - hole
contractSDVacuum (Particle x       ) (Hole     y           ) = ScalarZero
contractSDVacuum (Dagger   (Hole x)) (Particle y           ) = ScalarZero
contractSDVacuum (Hole     x       ) (Dagger   (Particle y)) = ScalarZero
-- The whole system should panic here
contractSDVacuum _                   _                       = undefined
