module QuantumField where

import Prelude hiding ((*), (+))

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
  | Oprod Operator Operator
  | Osum Operator Operator
  | Scaled Scalar Operator
  deriving(Show, Eq)

class Ring a where
  (*) :: a -> a -> a
  (+) :: a -> a -> a

instance Ring Operator where
  (*) = Oprod
  (+) = Osum

conj :: Scalar -> Scalar
conj (Delta s t)    = Delta s t
conj (Val  s   )    = Conj s
conj (Conj s   )    = Val s
conj ScalarZero     = ScalarZero
conj ScalarOne      = ScalarOne
conj (Sprod s t   ) = Sprod (conj s) (conj t)
conj (Ssum  s t   ) = Ssum (conj s) (conj t)
conj (Tensor s a i) = Tensor s a i

dag :: Operator -> Operator
dag (Dagger (Particle i)) = Particle i
dag (Dagger (Hole     a)) = Hole a
dag (Oprod  a b         ) = Oprod (dag b) (dag a)
dag (Scaled s o         ) = Scaled s (dag o)
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
isOneBody (Scaled   s p    ) = isOneBody p
isOneBody (Osum p q) = isOneBody p && isOneBody q
isOneBody  _ = False

normalize :: Operator -> Operator
normalize (Oprod (Op b) (Dagger (Op a))) = Scaled ParticleSign o
  where o = Oprod (Dagger (Op a)) (Op b)
normalize (Oprod b (Dagger (Op a))) = Scaled ParticleSign o
  where o = Oprod (Dagger (Op a)) (normalize b)
normalize (Oprod b (Op a)) = Oprod (normalize b) (Op a)
normalize (Oprod (Particle b) (Dagger (Particle a))) = Scaled ParticleSign o
  where o = Oprod (Dagger (Particle a)) (Particle b)
normalize (Oprod (Dagger (Hole a)) (Hole b)) = Scaled ParticleSign o
  where o = Oprod (Hole b) (Dagger (Hole a))
normalize (Osum   a b) = Osum (normalize a) (normalize b)
normalize (Scaled a b) = Scaled a (normalize b)
normalize a            = a

expand :: Operator -> Operator
expand (Oprod (Osum a b) c) = Osum (expand (Oprod a c)) (expand (Oprod b c))
expand (Oprod c (Osum a b)) = Osum (expand (Oprod c a)) (expand (Oprod c b))
--expand (Oprod a b) = Qprod (expand a) (expand b)
expand (Osum a b) = Osum (expand a) (expand b)
expand c = c

(^+) :: Operator -> Operator
(^+) = dag

instance Tex Operator where
  tex (Op       x           ) = "c_{" ++ x ++ "}"
  tex (Dagger   (Op x)      ) = "c^{" ++ x ++ "}"
  tex (Particle x           ) = "p_{" ++ x ++ "}"
  tex (Dagger   (Particle x)) = "p^{" ++ x ++ "}"
  tex (Hole     x           ) = "h_{" ++ x ++ "}"
  tex (Dagger   (Hole x)    ) = "h^{" ++ x ++ "}"
  tex FockOne = "1"
  tex FockZero = "0"
  tex (Scaled s p           ) = (tex s) ++ "*" ++ (tex p)
  tex (Oprod  o p           ) = (tex o) ++ (tex p)
  tex (Osum   o p           ) = (tex o) ++ " + " ++ (tex p)

instance Tex Scalar where
  tex (Delta x y   ) = "\\delta_{" ++ x ++ "," ++ y ++ "}"
  tex (Val  x      ) = "(" ++ (show x) ++ ")"
  tex (Conj x      ) = "(\\bar{" ++ (show x) ++ "})"
  tex ScalarOne = "1"
  tex ScalarZero = "0"
  tex (Sprod s t   ) = "(" ++ (tex s) ++ " * " ++ (tex t) ++ ")"
  tex (Ssum  s t   ) = "(" ++ (tex s) ++ " + " ++ (tex t) ++ ")"
  tex (Tensor s a i) = s ++ "^{" ++ a ++ "}_{" ++ i ++ "}"
  tex ParticleSign   = "ParticleSign"

contract :: Operator -> Operator -> Scalar

contract (Dagger   (Hole x)) (Hole     y           ) = Delta x y
contract (Hole     x       ) (Hole     y           ) = ScalarZero

contract (Particle x       ) (Dagger   (Particle y)) = Delta x y
contract (Particle x       ) (Particle y           ) = ScalarZero

contract (Particle x       ) (Hole     y           ) = ScalarZero
contract (Dagger   (Hole x)) (Particle y           ) = ScalarZero
contract (Hole     x       ) (Dagger   (Particle y)) = ScalarZero

contract (Oprod p q        ) (Oprod r s            ) = foldl
  Ssum
  ScalarZero
  [ Sprod (contract p q) (contract r s)
  , Sprod (contract p r) (contract q s)
  , Sprod (contract p s) (contract q r)
  ]
contract o (Oprod p s) = Ssum (contract o p) (contract o s)
contract (Scaled ScalarZero o) p           = ScalarZero
contract (Scaled s          o) p           = Sprod s (contract o p)
contract o                     p           = contract p o
