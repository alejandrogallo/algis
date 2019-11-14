module QuantumField where

type Index = String
type Field = Double

class Tex a where
  tex :: a -> String


data Scalar
  = Delta String String
  | Tensor String Index Index
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
  | Particle Index
  | Hole Index
  | Dagger Operator
  | Oprod Operator Operator
  | Osum Operator Operator
  | Scaled Scalar Operator
  deriving(Show, Eq)

conj :: Scalar -> Scalar
conj (Delta s t) = (Delta s t)
conj (Val s) = (Conj s)
conj (Conj s) = (Val s)
conj ScalarZero = ScalarZero
conj ScalarOne = ScalarOne
conj (Sprod s t) = (Sprod (conj s) (conj t))
conj (Ssum s t) = (Ssum (conj s) (conj t))
conj (Tensor s a i) = (Tensor s a i)

dag :: Operator -> Operator
dag (Dagger (Particle i)) = Particle i
dag (Dagger (Hole a)) = Hole a
dag (Oprod a b) = (Oprod (dag b) (dag a))
dag (Scaled s o) = (Scaled s (dag o))
dag FockOne = FockOne
dag FockZero = FockZero
dag o = Dagger o

(^+) :: Operator -> Operator
(^+) = dag

(#) :: Operator -> Operator -> Operator
(#) = Oprod

instance Tex Operator where
  tex (Particle x) = "p_{" ++ x ++ "}"
  tex (Dagger (Particle x)) = "p^{" ++ x ++ "}"
  tex (Hole x) = "h_{" ++ x ++ "}"
  tex (Dagger (Hole x)) = "h^{" ++ x ++ "}"
  tex (FockOne) = "1"
  tex (FockZero) = "0"
  tex (Scaled s p) = (tex s) ++ "*" ++ (tex p)
  tex (Oprod o p) = (tex o) ++ (tex p)
  tex (Osum o p) = (tex o) ++ " + " ++ (tex p)

instance Tex Scalar where
  tex (Delta x y) = "\\delta_{" ++ x ++ "," ++ y ++ "}"
  tex (Val x) = "(" ++ (show x) ++ ")"
  tex (Conj x) = "(\\bar{" ++ (show x) ++ "})"
  tex (ScalarOne) = "1"
  tex (ScalarZero) = "0"
  tex (Sprod s t) = "(" ++ (tex s) ++ " * " ++ (tex t) ++ ")"
  tex (Ssum s t) = "(" ++ (tex s) ++ " + " ++ (tex t) ++ ")"
  tex (Tensor s a i) = s ++ "^{" ++ a ++ "}_{"++ i ++ "}"

contract :: Operator -> Operator -> Scalar

contract (Dagger (Hole x)) (Hole y) = (Delta x y)
contract (Hole x) (Hole y) = ScalarZero

contract (Particle x) (Dagger (Particle y)) = (Delta x y)
contract (Particle x) (Particle y) = ScalarZero

contract (Particle x) (Hole y) = ScalarZero
contract (Dagger (Hole x)) (Particle y) = ScalarZero
contract (Hole x) (Dagger (Particle y)) = ScalarZero

contract (Oprod p q) (Oprod r s) =
   foldl Ssum ScalarZero [Sprod (contract p q) (contract r s),
                          Sprod (contract p r) (contract q s),
                          Sprod (contract p s) (contract q r)]
contract (o) (Oprod p s) = (Ssum (contract o p) (contract o s))
contract (Scaled ScalarZero o) (p) = ScalarZero
contract (Scaled s o) (p) = (Sprod s (contract o p))
contract o p = contract p o
