module QuantumField where


data Rat
  = Rat Int Int deriving (Show)

doubleGet :: Rat -> Maybe Double
doubleGet (Rat _ 0) = Nothing
doubleGet (Rat x y) = Just ((fromIntegral x) / (fromIntegral y))

--data Dagger = Dagger Operator

type Index = String

--data Delta = Delta String String deriving (Show)
data Scalar = Delta String String
            | Val Double
            deriving (Show)

data Operator = Particle Index
              | Hole Index
              | Dagger Operator
              | Productt [Operator]
              | Scaled Scalar Operator
              deriving(Show)

class Tex a where
  tex :: a -> String

instance Tex Operator where
  tex (Particle x) = "p_{" ++ x ++ "}"
  tex (Dagger (Particle x)) = "p^{" ++ x ++ "}"
  tex (Hole x) = "h_{" ++ x ++ "}"
  tex (Dagger (Hole x)) = "h^{" ++ x ++ "}"
  tex (Productt []) = ""
  tex (Productt (x:xs)) = tex x ++ tex (Productt xs)
  tex (Scaled s o) = tex s ++ " " ++ tex o

instance Tex Scalar where
  tex (Delta x y) = "\\delta_{" ++ x ++ "," ++ y ++ "}"
  tex (Val x) = "(" ++ (show x) ++ ")"


contract :: Operator -> Operator -> Scalar
contract (Hole x) (Dagger (Hole y)) = (Delta x y)
contract (Particle x) (Dagger (Particle y)) = (Delta x y)
contract (_) (_) = Val 0.0

