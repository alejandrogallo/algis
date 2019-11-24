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

class Tex a where
  tex :: a -> String

data Scalar
  = Delta String String
  | Tensor String Index Index
  | ParticleSign
  | ScalarZero
  | ScalarOne
  | Val Field
  | Conj Field
  | Sprod Scalar Scalar
  | Ssum Scalar Scalar
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

class Ring a where
  (*) :: a -> a -> a
  (**) :: a -> Int -> a
  (+) :: a -> a -> a

instance Ring Operator where
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
conj (Val  s      ) = Conj s
conj (Conj s      ) = Val s
conj (Sprod s t   ) = Sprod (conj s) (conj t)
conj (Ssum  s t   ) = Ssum (conj s) (conj t)
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
--normalize (Oprod (Op b) (Dagger (Op a))) = Scaled ParticleSign o
  --where o = Oprod (Dagger (Op a)) (Op b)
--normalize (Oprod b (Dagger (Op a))) = Scaled ParticleSign o
  --where o = Oprod (Dagger (Op a)) (normalize b)
--normalize (Oprod b (Op a)) = Oprod (normalize b) (Op a)
--normalize (Oprod (Particle b) (Dagger (Particle a))) = Scaled ParticleSign o
  --where o = Oprod (Dagger (Particle a)) (Particle b)
--normalize (Oprod (Dagger (Hole a)) (Hole b)) = Scaled ParticleSign o
  --where o = Oprod (Hole b) (Dagger (Hole a))
--normalize (Osum   a b) = Osum (normalize a) (normalize b)
normalize (Scaled a b) = Scaled a (normalize b)
normalize a            = a

expand :: Operator -> Operator
expand (Oprod c   ) = Osum $ map Oprod (expandList c)
expand (Osum  a   ) = foldl (+) FockZero (map (expand . Oprod . (: [])) a)
--expand (Osum a) = Osum $ map (expandList . (:[])) a
expand (Scaled s a) = Osum [Scaled s (expand a)]
expand c            = Osum [c]

expandList :: [Operator] -> [[Operator]]
expandList l@(x : xs) = case x of
  (Oprod a) -> expandList $ a ++ xs
  (Osum  a) -> mconcat $ map expandList $ [(:)] <*> a <*> [xs]
  _         -> map (x :) (expandList xs)
expandList [] = [[]]

instance Tex Operator where
  tex (Op       x           ) = "c_{" ++ x ++ "}"
  tex (Dagger   (Op x)      ) = "c^{" ++ x ++ "}"
  tex (Particle x           ) = "p_{" ++ x ++ "}"
  tex (Dagger   (Particle x)) = "p^{" ++ x ++ "}"
  tex (Hole     x           ) = "h_{" ++ x ++ "}"
  tex (Dagger   (Hole x)    ) = "h^{" ++ x ++ "}"
  tex FockOne                 = "1"
  tex FockZero                = "0"
  tex (Scaled s p)            = tex s ++ "*" ++ tex p
  tex (Oprod o   )            = mconcat $ map tex o
  tex (Osum  o   )            = intercalate " + " $ map tex o

instance Tex Scalar where
  tex (Delta x y)    = "\\delta_{" ++ x ++ "," ++ y ++ "}"
  tex (Val  x   )    = "(" ++ show x ++ ")"
  tex (Conj x   )    = "(\\bar{" ++ show x ++ "})"
  tex ScalarOne      = "1"
  tex ScalarZero     = "0"
  tex (Sprod s t   ) = "(" ++ tex s ++ " * " ++ tex t ++ ")"
  tex (Ssum  s t   ) = "(" ++ tex s ++ " + " ++ tex t ++ ")"
  tex (Tensor s a i) = s ++ "^{" ++ a ++ "}_{" ++ i ++ "}"
  tex ParticleSign   = "ParticleSign"

contract :: Operator -> Operator -> Scalar

contract (Dagger   (Hole x)  ) (Hole     y           ) = Delta x y
contract (Hole     x         ) (Hole     y           ) = ScalarZero

contract (Particle x         ) (Dagger   (Particle y)) = Delta x y
contract (Particle x         ) (Particle y           ) = ScalarZero

contract (Particle x         ) (Hole     y           ) = ScalarZero
contract (Dagger   (Hole x)  ) (Particle y           ) = ScalarZero
contract (Hole     x         ) (Dagger   (Particle y)) = ScalarZero

--contract (Oprod p q        ) (Oprod r s            ) = foldl
  --Ssum
  --ScalarZero
  --[ Sprod (contract p q) (contract r s)
  --, Sprod (contract p r) (contract q s)
  --, Sprod (contract p s) (contract q r)
  --]
--contract o (Oprod p s) = Ssum (contract o p) (contract o s)
contract (Scaled ScalarZero o) p                       = ScalarZero
contract (Scaled s          o) p                       = Sprod s (contract o p)
contract o                     p                       = contract p o
