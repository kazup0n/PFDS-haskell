module SprayHeapSpec
  ( spec
  ) where

import           Lib
import           SprayHeap
import           Test.Hspec

insertNums :: [Int] -> Tree Int
insertNums = foldl (\t x -> insert x t) E

spec :: Spec
spec = do
  describe "insert" $ do
    it "insert into empty tree" $ insert 1 E `shouldBe` T E 1 E
    it "insert smaller values" $ insertNums [5, 4, 3, 2, 1] `shouldBe` T E 1 (T E 2 (T E 3 (T E 4 (T E 5 E))))
    it "insert smaller values" $
      (insertNums . reverse) [5, 4, 3, 2, 1] `shouldBe` T (T (T (T (T E 1 E) 2 E) 3 E) 4 E) 5 E
