module Spec where
import PdePreludat
import Library
import Test.Hspec

correrTests :: IO ()
correrTests = hspec $ do
  describe "Platos" $ do
      it "un plato contiene un ingrediente que se le agregó" $ do platoContiene (agregarComponente (Plato 5 []) (crearComponente "Sal" 3)) "Sal" `shouldBe` True
