{-# LANGUAGE BlockArguments #-}

module Block3Test
  ( test
  ) where

import Block3
import Test.Tasty (TestTree)
import Test.Tasty.Hspec (describe, it, shouldBe, testSpec)
import Data.List.NonEmpty


test :: IO TestTree
test =
  testSpec "Block3" $ do
    describe "maybeConcat" $ do
      it "maybeConcat [Just [1,2,3], Nothing, Just [4,5]] = [1, 2, 3, 4, 5]" $ maybeConcat [Just [1,2,3], Nothing, Just [4,5]] `shouldBe` [1, 2, 3, 4, 5]
      it "maybeConcat [Just [1,2,3], Nothing, Just [4,5], Just [1,2,3], Nothing, Just [4,5]] = [1,2,3,4,5,1,2,3,4,5]" $ maybeConcat [Just [1,2,3], Nothing, Just [4,5], Just [1,2,3], Nothing, Just [4,5]] `shouldBe` [1,2,3,4,5,1,2,3,4,5]
      
    describe "Semigroup thisOrThat" $ do
      it "This [19] <> This [45] == This [19,45]" $
       ((This [19] :: ThisOrThat [Int] [Int]) <> (This [45] :: ThisOrThat [Int] [Int])) `shouldBe` This [19,45]
      it "That [19] <> This [45] == That [19,45]" $
       ((That [19] :: ThisOrThat [Int] [Int]) <> (That [45] :: ThisOrThat [Int] [Int])) `shouldBe` That [19,45]
      it "That [19] <> This [45] == This [19,45]" $
       ((That [19] :: ThisOrThat [Int] [Int]) <> (This [45] :: ThisOrThat [Int] [Int])) `shouldBe` Both [45] [19]
      it "This [19] <> This [45] == That [19,45]" $
       ((This [19] :: ThisOrThat [Int] [Int]) <> (That [45] :: ThisOrThat [Int] [Int])) `shouldBe` Both [19] [45]
      it "This [19] <> This [45] == That [19,45]" $
       ((This [19] :: ThisOrThat [Int] [Int]) <> (Both [5] [45] :: ThisOrThat [Int] [Int])) `shouldBe` Both [19,5] [45]
      it "This [19] <> This [45] == That [19,45]" $
       ((That [19] :: ThisOrThat [Int] [Int]) <> (Both [5] [45] :: ThisOrThat [Int] [Int])) `shouldBe` Both [5] [19,45]
      it "This [19] <> This [45] == That [19,45]" $
       ((Both [5] [45] :: ThisOrThat [Int] [Int]) <> (That [45] :: ThisOrThat [Int] [Int])) `shouldBe` Both [5] [45,45]
      it "This [19] <> This [45] == That [19,45]" $
       ((Both [5] [45] :: ThisOrThat [Int] [Int]) <> (This [45] :: ThisOrThat [Int] [Int])) `shouldBe` Both [5,45] [45]
      it "This [19] <> This [45] == That [19,45]" $
       ((Both [5] [45] :: ThisOrThat [Int] [Int]) <> (Both [21] [20] :: ThisOrThat [Int] [Int])) `shouldBe` Both [5,21] [45,20]