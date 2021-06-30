module StackSpec (spec) where

import Test.Hspec
import Lib
import Stack
  
spec :: Spec 
spec = do 
  describe "suffixes" $ do
    it "returns suffex list" $
        (suffixes (LS [1,2,3,4])) `shouldBe` (LS [(LS [1,2,3,4]), (LS [2,3,4]), (LS [3,4]), (LS [4])])

