module WHeapSpec (spec) where
import Test.Tasty.Hspec
import Lib
import WHeap
import Heap(insert)
  
spec :: Spec 
spec = do 
  describe "insert" $ do
    it "returns empty node when insert into E" $
        insert 1 E `shouldBe` T 1 E E
    it "insert 2" $
      insert 2 (insert 1 E) `shouldBe` T 1 (T 2 E E) E
    it "insert 5" $
      insert 5 (insert 2 (insert 1 E)) `shouldBe` T 1 (T 5 E E) (T 2 E E)
    it "insert 3" $
      insert 3 (insert 5 (insert 2 (insert 1 E))) `shouldBe` T 1 (T 2 (T 3 E E) E) (T 5 E E)

