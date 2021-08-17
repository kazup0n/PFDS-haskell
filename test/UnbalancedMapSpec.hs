module UnbalancedMapSpec (spec) where

import Test.Hspec
import UnbalancedMap(UnbalancedMap(..))
import FiniteMap
  
type StringMap = UnbalancedMap String String
emptyMap::StringMap
emptyMap = empty


spec :: Spec 
spec = do 
  describe "UnbalancedMap empty" $ do
    it "returns empty map" $
      emptyMap `shouldBe` emptyMap
  
  describe "lookup" $ do
    it "returns value when key presents" $
      FiniteMap.lookup "a" (bind "a" "abc" emptyMap) `shouldBe` Just "abc"
    it "returns Nothing when key absent" $
      FiniteMap.lookup "b" (bind "a" "abc" emptyMap) `shouldBe` Nothing
    it "returns Nothing for emptyMap" $
      FiniteMap.lookup "key" emptyMap `shouldBe` Nothing
