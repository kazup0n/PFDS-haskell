module RBTreeSpec (spec) where

import Test.Hspec
import Lib
import RBTree
  
spec :: Spec 
spec = do 
  describe "fromOrdList" $ do
    it "returns empty Tree when list is empty" $
        fromOrdList ([] :: [Int]) `shouldBe` E
    it "returns ... Tree when list is [1,2,3,4]" $
        fromOrdList [1,2,3,4] `shouldBe` T B (T B E 1 E) 2 (T B E 3 (T R E 4 E))
    it "returns ... Tree when list is reversed [1,2,3,4]" $
        (fromOrdList . reverse)  [1,2,3,4] `shouldBe` T B (T B (T R E 1 E) 2 E) 3 (T B E 4 E)


