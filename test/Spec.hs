import Test.Hspec

import Language.Trout.Interpreter.Store

main :: IO ()
main = hspec $ do
  describe "Interpreter.State.VarStorage" $ do
    it "can store and retrieve Vars" $ do
      var `shouldBe` (24 :: Int) where
        store = updateVarInVarStore [] "bar" 24 False []

        var :: Int
        var = getVarInVarStore store "bar"
