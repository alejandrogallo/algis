import Test.Hspec
import QuantumField


main :: IO ()
main = hspec $ do
  describe "TeX output" $ do
    it "Correctly outputs all particles and holes" $ do
      (tex $ Particle "a") `shouldBe` "p_{a}"
      (tex $ Hole "a") `shouldBe` "h_{a}"
      (tex $ Dagger $ Hole "a") `shouldBe` "h^{a}"

  --print "Hello world"
  --print $ Rat 1 2
  --print $ doubleGet $ Rat 1 2
  --print $ doubleGet $ Rat 1 0
  --print $ doubleGet $ Rat 0 2
  --print $ doubleGet $ Rat 0 0
  --print $ Particle "a"
  --print $ Hole "i"
  ----putStrLn $ tex $ Delta "a" "b"
  --putStrLn $ tex $ Scaled (Val 1.0) (Particle "a")
  --putStrLn $ tex $ Dagger $ Hole "i"
  --putStrLn $ tex $ Productt $ [(Hole "i"), (Particle "a")]
  --print $ contract (Dagger $ Particle "a") (Hole "i")
  --print $ contract (Particle "a") (Hole "i")
  --print $ contract (Particle "a") (Dagger $ Hole "i")
  --print $ contract (Particle "a") (Dagger $ Particle "b")
  --print $ contract (Hole "i") (Dagger $ Hole "j")
