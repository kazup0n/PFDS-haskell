module HeapSpec (spec) where

import Test.Hspec
import Lib
import Heap
  
spec :: Spec 
spec = do 
  describe "insert2" $ do
    it "returns empty node when insert into E" $
        insert2 1 E `shouldBe` insert 1 E
    it "insert 2" $
      insert2 2 (insert2 1 E) `shouldBe` insert 2 (insert 1 E)
    it "insert 5" $
      insert2 5 (insert2 2 (insert2 1 E)) `shouldBe` insert 5 (insert 2 (insert 1 E))
    it "insert 3" $
      insert2 3 (insert2 5 (insert2 2 (insert2 1 E))) `shouldBe` insert 3 (insert 5 (insert 2 (insert 1 E)))

