{-# LANGUAGE BlockArguments #-}

module Block1Test
  ( test
  ) where

import Block1
import Test.Tasty (TestTree)
import Test.Tasty.Hspec
import Test.Tasty.QuickCheck
import Text.Read (readMaybe)

test :: IO TestTree
test =
  testSpec "HW2 Block1" $ do
    describe "stringSum unit" $ do
      it "Sum of int sum" $ stringSum "1 2 3 4 5" `shouldBe` Just 15
      it "Sum of negative" $ stringSum "-1 -2 -3 -4 -5" `shouldBe` Just (-15)
      it "Sum of negative with whitespaces" $ stringSum "-1      -2          \n       \r\r\t\n          -3      -4 -5" `shouldBe` Just (-15)
      it "Sum of negative and 'a'" $ stringSum "-1 -2 -3 a -4 -5" `shouldBe` Nothing
      it "Sum of empty string" $ stringSum "" `shouldBe` Nothing
      it "Sum of one element" $ stringSum "1" `shouldBe` Just 1

    describe "stringSum property" $ do
      it "Sum of int sum is just" $ property $ (stringSum "1 2 3 4 5" == (Just 15 :: Maybe Int))
      it "Sum of negative is just" $ property $ (stringSum "-1 -2 -3 -4 -5" == (Just (-15) :: Maybe Int))
      it "Sum of negative with whitespaces is just" $ property $ (stringSum "-1      -2          \n       \r\r\t\n          -3      -4 -5" == (Just (-15) :: Maybe Int))
      it "Sum of negative and 'a' is Nothing" $ property $ (stringSum "-1 -2 -3 a -4 -5" == (Nothing :: Maybe a))
      it "Sum of empty string is Nothing" $ property $ (stringSum "" == (Nothing :: Maybe a))
      it "Sum of one element is just" $ property $ (stringSum "1" == (Just 1 :: Maybe Int))


    describe "Tree functor" $ do
      it "(+2) and Leaf" $ show (fmap (+2) (Leaf 1)) `shouldBe` "Leaf: 3"
      it "(+2) and Branch" $ show (fmap (+2) (Branch (Leaf 0) (Branch (Leaf 1) (Leaf 2)))) `shouldBe` "(Left: Leaf: 2) (Right: (Left: Leaf: 3) (Right: Leaf: 4))"

    describe "Tree applicative" $ do
      it "Leaf (+2) and Leaf" $ show (Leaf (+2) <*> Leaf 1) `shouldBe` "Leaf: 3"
      it "Leaf (+2) and Branch" $ show (Leaf (+2) <*> (Branch (Leaf 1) (Leaf 2))) `shouldBe` "(Left: Leaf: 3) (Right: Leaf: 4)"

    describe "Tree foldable" $ do
      it "foldMap with Leaf" $ show (foldMap (\x -> [x]) (Leaf 1)) `shouldBe` "[1]"
      it "foldMap with Branch" $ show (foldMap (\x -> [x]) (Branch (Leaf 1) (Leaf 2))) `shouldBe` "[1,2]"

    describe "Tree traverseble" $ do
      it "traverse with Leaf" $ fmap sum (traverse readMaybe (Leaf "1")) `shouldBe` Just 1
      it "traverse with Branch" $ fmap sum (traverse readMaybe (Branch (Leaf "1") (Leaf "2"))) `shouldBe` Just 3

    describe "Nonempty functor" $ do
      it "foldMap with 0 :| [1, 2, 3]" $ show (fmap (+1) (0 :| [1, 2, 3])) `shouldBe` "1 :| [2,3,4]"
      it "foldMap with 0 :| []" $ show (fmap (+1) (0 :| [])) `shouldBe` "1 :| []"

    describe "Nonempty applicative" $ do
      it "((+1) :| [(*(-2)), (*3)]) <*> (1 :| [])" $ show (((+1) :| [(*(-2)), (*3)]) <*> (1 :| [])) `shouldBe` "2 :| []"
      it "((+1) :| [(*(-2)), (*3)]) <*> (1 :| [2, 3, 4])" $ show (((+1) :| [(*(-2)), (*3)]) <*> (1 :| [2, 3, 4])) `shouldBe` "2 :| [3,4,5,-4,-6,-8,6,9,12]"

    describe "Nonempty foldable" $ do
      it "foldMap with (1 :| [2, 3, 4])" $ show (foldMap (\x -> [x]) (1 :| [2, 3, 4])) `shouldBe` "[1,2,3,4]"
      it "foldMap with (1 :| [])" $ show (foldMap (\x -> [x]) (1 :| [])) `shouldBe` "[1]"

    describe "Nonempty traverseble" $ do
      it "foldMap with (1 :| [2, 3, 4])" $ fmap sum (traverse readMaybe ("1" :| ["2", "3", "4"])) `shouldBe` Just 10
