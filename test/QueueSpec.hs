module QueueSpec
  ( spec
  ) where

import           Lib
import           Queue      (Deq (Deq), Deque (cons, init, last),
                             Queue (empty, head, snoc, tail))
import           Test.Hspec

intQueue :: Deq Int
intQueue = empty

snocAll :: Deq a -> [a] -> Deq a
snocAll q as = foldr (\a q -> snoc q a) q as

spec :: Spec
spec = do
  describe "empty" $ do
    it "returns an empty queue" $ intQueue `shouldBe` (Deq [] [])
  describe "snoc" $ do
    it "when queue is empty, returns queue which has a elem added in head" $
      snoc intQueue 1 `shouldBe` (Deq [1] [])
    it "when queue is not empty, returns queue has a elem added in tail " $
      snoc (snoc (snoc (snoc intQueue 1) 2) 3) 4 `shouldBe` (Deq [1] [4, 3, 2])
  describe "head" $ do
    it "returns Nothing when queue is empty" $
      Queue.head intQueue `shouldBe` Nothing
    it "returns Just x when queue is not empty" $
      Queue.head (Deq [1] [4, 3, 2]) `shouldBe` Just 1
  describe "tail" $ do
    it "returns Nothing when queue is empty" $
      Queue.tail intQueue `shouldBe` Nothing
    it "returns a queue when f, r of queue is not empty" $
      Queue.tail (Deq [1] [4, 3, 2]) `shouldBe` Just (Deq [2, 3, 4] [])
    it "returns a queue when f, r of queue is not empty" $
      Queue.tail (Deq [2, 3, 4] []) `shouldBe` Just (Deq [3, 4] [])
    it "returns a empty queue when f consists of one element" $
      Queue.tail (Deq [2] []) `shouldBe` Just (Deq [] [])
  describe "cons" $ do
    it "returns queue which has added element in f" $
      cons (cons (cons intQueue 1) 2) 3 `shouldBe` Deq [3, 2, 1] []
  describe "last" $ do
    it "returns Nothing when queue is empty" $
      Queue.last intQueue `shouldBe` Nothing
    it "returns Nothing when r of queue is empty" $
      Queue.last (Deq [1] []) `shouldBe` Nothing
    it "returns x when r of queue is not empty" $
      Queue.last (Deq [1] [2]) `shouldBe` Just 2
  describe "init" $ do
    it "returns nothing" $ Queue.init (intQueue) `shouldBe` Nothing
    it "returns empty queue" $
      Queue.init (Deq [] [1]) `shouldBe` Just (Deq [] [])
    it "returns queue with last element removed" $
      Queue.init (Deq [1] [4, 5, 6]) `shouldBe` Just (Deq [1] [5, 6])
