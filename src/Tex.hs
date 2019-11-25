module Tex where

import           QuantumField
import           Data.List

class Tex a where
  tex :: a -> String

instance Tex Operator where
  tex (Op       x           ) = "c_{" ++ x ++ "}"
  tex (Dagger   (Op x)      ) = "c^{" ++ x ++ "}"
  tex (Particle x           ) = "p_{" ++ x ++ "}"
  tex (Dagger   (Particle x)) = "p^{" ++ x ++ "}"
  tex (Hole     x           ) = "h_{" ++ x ++ "}"
  tex (Dagger   (Hole x)    ) = "h^{" ++ x ++ "}"
  tex FockOne                 = "1_{F}"
  tex FockZero                = "0_{F}"
  tex (Scaled s p)            = tex s ++ "*" ++ tex p
  tex (Oprod o   )            = mconcat $ map tex o
  tex (Osum  o   )            = intercalate " + " $ map tex o

instance Tex Scalar where
  tex (Delta x y)    = "\\delta_{" ++ x ++ "," ++ y ++ "}"
  tex (Val  x   )    = "(" ++ show x ++ ")"
  tex (Conj x   )    = "(\\bar{" ++ show x ++ "})"
  tex ScalarOne      = "1"
  tex ScalarZero     = "0"
  tex (Sprod s     ) = "(" ++ (intercalate "*" $ map tex s) ++ ")"
  tex (Ssum  s     ) = "(" ++ (intercalate " + " $ map tex s) ++ ")"
  tex (Tensor s a i) = s ++ "^{" ++ a ++ "}_{" ++ i ++ "}"
  tex ParticleSign   = "ParticleSign"

