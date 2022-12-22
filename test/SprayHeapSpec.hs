module SprayHeapSpec
  ( spec,
  )
where

import Lib
import SprayHeap
import Test.Hspec

insertNums :: [Int] -> Tree Int
insertNums = foldl (flip insert) E

inner :: (Tree Int, [Int]) -> Int -> (Tree Int, [Int])
inner (t, a) x = insert2 x t

insertNums2 :: [Int] -> (Tree Int, [Int])
insertNums2 = foldl inner (E, [])

spec :: Spec
spec = do
  describe "insert" $ do
    it "insert into empty tree" $ insert 1 E `shouldBe` T E 1 E
    it "insert smaller values" $ insertNums [5, 4, 3, 2, 1] `shouldBe` T E 1 (T E 2 (T E 3 (T E 4 (T E 5 E))))
    it "insert smaller values" $
      (insertNums . reverse) [5, 4, 3, 2, 1] `shouldBe` T (T (T (T (T E 1 E) 2 E) 3 E) 4 E) 5 E

  describe "insert2" $ do
    it "insert into empty tree" $ insert2 1 E `shouldBe` (T E 1 E, [])
    it "insert 1,2" $ insertNums2 [1, 2] `shouldBe` (T (T E 1 E) 2 E, [1])
    it "insert 1,2,3" $ insertNums2 [1, 2, 3] `shouldBe` (T (T (T E 1 E) 2 E) 3 E, [2])
    it "insert 1,2,3,4" $ insertNums2 [1, 2, 3, 4] `shouldBe` (T (T (T (T E 1 E) 2 E) 3 E) 4 E, [3])
    it "insert 2,1" $ insertNums2 [2, 1] `shouldBe` (T E 1 (T E 2 E), [2])
    it "insert 3,2,1" $ insertNums2 [3, 2, 1] `shouldBe` (T E 1 (T E 2 (T E 3 E)), [2])
    it "insert 4,3,2,1" $ insertNums2 [4, 3, 2, 1] `shouldBe` (T E 1 (T E 2 (T E 3 (T E 4 E))), [2])
    it "insert 4,3" $ insertNums2 [4, 3] `shouldBe` (T E 3 (T E 4 E),[4])
    it "insert 4,3,1" $ insertNums2 [4, 3, 1] `shouldBe` (T E 1 (T E 3 (T E 4 E)), [3])
    it "insert 4,3,1,2" $ insertNums2 [4, 3, 1, 2] `shouldBe` (T (T E 1 E) 2 (T E 3 (T E 4 E)),[1,3])