module StreamSpec (spec) where

import Test.Hspec
import Lib
import Stream
  
spec :: Spec 
spec = do 
  let nil :: Stream Int
      nil = Nil
  describe "sort" $ do
    it "returns Nil when stream is Nil" $
        (sort nil) `shouldBe` nil
    it "returns stream itself when stream is (Cons 0 Nil)" $
        sort (Cons 1 Nil) `shouldBe` (Cons 1 Nil)
    it "returns sorted stream " $
        sort (Cons 3 (Cons 2 (Cons 1 Nil))) `shouldBe` (Cons 1 (Cons 2 (Cons 3 Nil)))
    it "returns sorted stream 2 " $
        sort (Cons 4 (Cons 5 (Cons 1 (Cons 8 Nil)))) `shouldBe`  (Cons 1 (Cons 4 (Cons 5 (Cons 8 Nil)))) 
    it "returns sorted stream 3 " $
        sort (Cons 4 (Cons 5 (Cons 1 (Cons 8 (Cons (-1) Nil))))) `shouldBe`  (Cons (-1) (Cons 1 (Cons 4 (Cons 5 (Cons 8 Nil)))) )
