import Test.Hspec
import QuantumField

p idx = (Particle idx)
p' idx = (dag (Particle idx))
h idx = (Hole idx)
h' idx = (dag (Hole idx))
tai a i = Scaled (Tensor "t" a i) (p' a) # (h i)
tabij a b i j = Scaled (Tensor "t" (a++b) (i++j)) (p' a) # (p' b) # (h j) # (h i)
fij i j = Scaled (Tensor "f" i j) (h' j) # (h i)
fab a b = Scaled (Tensor "f" a b) (p' a) # (p b)
tauia i a = (h' i) # (p a)
tauai a i = (p' a) # (h i)

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
      a_i = (dag a) # i
      i_a = (dag i) # a
      tek = Scaled (Tensor "t" "b" "k") (dag b) # k

  describe "TeX output" $ do
    it "Correctly outputs all particles and holes" $ do
      (tex $ a) `shouldBe` "p_{a}"
      (tex $ i) `shouldBe` "h_{i}"
      (tex $ dag i) `shouldBe` "h^{i}"
      (tex $ ((^+) a) # i) `shouldBe` "p^{a}h_{i}"
      (tex $ tai "e" "k") `shouldBe` "t^{e}_{k}*p^{e}h_{k}"

  describe "Contraction" $ do
    it "Correctly contracts holes and particles" $ do
      (tex $ contract (h' "i") (h "j")) `shouldBe` "\\delta_{i,j}"
      (tex $ contract i j) `shouldBe` "0"
      (tex $ contract a b) `shouldBe` "0"
      (tex $ contract b (dag a)) `shouldBe` "\\delta_{b,a}"
      (tex $ contract i (dag a)) `shouldBe` "0"
    it ("Correctly contracts " ++ (tex a_i) ++ " and " ++ (tex i) ++ " particles") $ do
      (tex $ contract (a_i) i) `shouldBe` "(0 + 0)"
    it ("Correctly contracts " ++ (tex a_i) ++ " and " ++ (tex i_a)) $ do
      (tex $ contract a_i i_a) `shouldBe` "(((0 + (0 * 0)) + (0 * 0)) + (0 * 0))"
    it ("Correctly contracts " ++ (tex i_a) ++ " and " ++ (tex a_i)) $ do
      (tex $ contract i_a a_i) `shouldBe` "(((0 + (0 * 0)) + (0 * 0)) + (\\delta_{i,i} * \\delta_{a,a}))"
      (tex $ contract (tauia "i" "a") ((fij "k" "l") # (tai "a" "m"))) `shouldBe` "asdf"
