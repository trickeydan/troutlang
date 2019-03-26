import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

import Language.Trout.Interpreter.State

main :: IO ()
main = hspec $ do
  describe "Interpreter.State.VarStorage" $ do
    it "can store and retrieve Vars" $ do
      retrieve var `shouldBe` (24 :: Int) where
        store = updateVarInStore [] "bar" 24 False []

        var :: Maybe Int
        var = getVarInStore store "bar"

        retrieve  :: Maybe Int -> Int
        retrieve  (Just x) = x
        retrieve  Nothing = -1
            
