import Test.Hspec
import QuantumField
import Prelude hiding ((*), (**), (+))

o = Op
p = Particle
p' idx = dag (Particle idx)
h = Hole
h' idx = dag (Hole idx)
tai a i = Scaled (Tensor "t" a i) $ p' a * h i
tabij a b i j = Scaled (Tensor "t" (a++b) (i++j)) $ p' a * p' b * h j * h i
fij i j = Scaled (Tensor "f" i j) $ h' j * h i
fab a b = Scaled (Tensor "f" a b) $ p' a * p b
tauia i a = h' i * p a
tauai a i = p' a * h i

main :: IO ()
main = hspec $ do
  let a = p "a"
      b = p "b"
      c = p "c"
      d = p "d"
      e = p "e"
      f = p "f"
      g = p "f"
      i = h "i"
      j = h "j"
      k = h "k"
      l = h "l"
      m = h "m"
      n = h "n"
      a_i = dag a * i
      i_a = dag i * a
      tek = Scaled (Tensor "t" "b" "k") $ dag b * k

  describe "TeX output" $ do
    it "Correctly outputs all particles and holes" $ do
      tex (o "a") `shouldBe` "c_{a}"
      (tex . dag $ o "a") `shouldBe` "c^{a}"
      tex a `shouldBe` "p_{a}"
      tex i `shouldBe` "h_{i}"
      tex (dag i) `shouldBe` "h^{i}"
      tex (dag a * i) `shouldBe` "p^{a}h_{i}"
      tex (a * b * c) `shouldBe` "p_{a}p_{b}p_{c}"
      tex (a*i + b + c) `shouldBe` "p_{a}h_{i} + p_{b} + p_{c}"
      tex (a*dag i + b + c) `shouldBe` "p_{a}h^{i} + p_{b} + p_{c}"
      tex (tai "e" "k") `shouldBe` "t^{e}_{k}*p^{e}h_{k}"

  describe "Dagger function" $ do
    it "Correctly handles the trivial cases" $ do
      dag (a * i) `shouldBe` (dag i) * (dag a)
      dag (a * i * j) `shouldBe` (dag j) * (dag i) * (dag a)
      dag (a + i * j) `shouldBe` (dag a) + ((dag j) * (dag i))

  describe "Build sums and products" $ do
    it "General" $ do
      a*c `shouldBe` Oprod [a, c]
      (a + b)*c `shouldBe` Oprod [a+b, c]
      c*(a + b) `shouldBe` Oprod [c, a+b]
      c*a + b `shouldBe` Osum [c*a, b]
      (a + b + c + d) `shouldBe` Osum [a,b,c,d]
      (a ** 3) `shouldBe` Oprod [a,a,a]
      (a + i*j) `shouldBe` Osum [a,i*j]

  describe "Expanding operators" $ do
    it "trivial expand" $ do
      expand a `shouldBe` Osum [a]
      expand (a + b) `shouldBe` (Oprod [a]) + (Oprod [b])
      expand (a + b + c) `shouldBe` (Oprod [a]) + (Oprod [b]) + (Oprod [c])
      expand (a*b) `shouldBe` Osum [a*b]
      expand ((a*b) + (b*i) + c) `shouldBe` Osum [a*b, b*i, Oprod [c]]
    it "Expands sums (a+b)**2" $ do
      expand ((a + b)**2) `shouldBe` Osum [a**2, a*b, b*a, b**2]
    it "Expands sums ((a+b)**2)+" $ do
      (dag . expand)  ((a + b)**2)
       `shouldBe`
       Osum [(dag a)**2, (dag b)*(dag a), (dag a)*(dag b), (dag b)**2]
    it "Expands sums (a+b)*c" $ do
      expand ((a + b)*c) `shouldBe` Osum [a*c, b*c]
    it "Expands sums c*(a+b)" $ do
      expand (c*(a + b)) `shouldBe` (c*a)+(c*b)
    it "Expands sums (a+b)*(c+d)" $ do
      expand ((a + b)*(c + d)) `shouldBe` ((a*c)+(a*d))+((b*c)+(b*d))
    it "Expands sums (a+b)*(c+d)*i" $ do
      expand ((a + b)*(c + d)*i) `shouldBe` Osum [a*c*i,a*d*i,b*c*i,b*d*i]
    it "Expands sums (a+b)*i*(c+d)" $ do
      expand ((a + b)*i*(c + d)) `shouldBe` Osum [a*i*c,a*i*d,b*i*c,b*i*d]
    it "Expands sums i*(a+b)*(c+d)" $ do
      expand (i*(a + b)*(c + d)) `shouldBe` Osum [i*a*c,i*a*d,i*b*c,i*b*d]
    it "Expands sums (a+b)*(c+d)*(i+j)" $ do
      expand ((a + b)*(c + d)*(i+j))
        `shouldBe`
        Osum [a*c*i,a*c*j,a*d*i,a*d*j, b*c*i,b*c*j,b*d*i,b*d*j]
{-

  describe "Contraction" $ do
    it "Correctly contracts holes and particles" $ do
      (tex $ contract (h' "i") (h "j")) `shouldBe` "\\delta_{i,j}"
      (tex $ contract i j) `shouldBe` "0"
      (tex $ contract a b) `shouldBe` "0"
      (tex $ contract b (dag a)) `shouldBe` "\\delta_{b,a}"
      (tex $ contract i (dag a)) `shouldBe` "0"
    it ("Correctly contracts " ++ (tex a_i) ++ " and " ++ (tex i) ++ " particles") $ do
      (tex $ contract (a_i) i) `shouldBe` "(0 + 0)"
    --it ("Correctly contracts " ++ (tex a_i) ++ " and " ++ (tex i_a)) $ do
      --(tex $ contract a_i i_a) `shouldBe` "(((0 + (0 * 0)) + (0 * 0)) + (0 * 0))"
    --it ("Correctly contracts " ++ (tex i_a) ++ " and " ++ (tex a_i)) $ do
      --(tex $ contract i_a a_i) `shouldBe` "(((0 + (0 * 0)) + (0 * 0)) + (\\delta_{i,i} * \\delta_{a,a}))"
      --(tex $ contract (tauia "i" "a") ((fij "k" "l") * (tai "a" "m"))) `shouldBe` "asdf"

-}
