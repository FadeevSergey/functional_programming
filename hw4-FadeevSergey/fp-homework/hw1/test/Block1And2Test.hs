{-# LANGUAGE BlockArguments #-}

module Block1And2Test
  ( test
  ) where

import Block1And2
import Test.Tasty (TestTree)
import Test.Tasty.Hspec (describe, it, shouldBe, testSpec)

test :: IO TestTree
test =
  testSpec "Block1And2" $ do
    describe "nextDay" $ do
      it "Tuesday after Mondey" $ nextDay Monday `shouldBe` Tuesday
      it "Monday after Sunday" $ nextDay Sunday `shouldBe` Monday

    describe "afterDays" $ do
      it "Sunday + 0 = Sunday" $ Sunday `afterDays` 0 `shouldBe` Sunday
      it "Saturday + 3 = Tuesday" $ Saturday `afterDays` 3 `shouldBe` Tuesday
      it "Thursday + 6 = Wednesday" $ Thursday `afterDays` 6 `shouldBe` Wednesday

    describe "isWeekend" $ do
      it "Saturday is a weekend" $ isWeekend Saturday `shouldBe` True
      it "Sunday is a weekend" $ isWeekend Sunday `shouldBe` True
      it "Friday is not a weekend" $ isWeekend Friday `shouldBe` False

    describe "daysToParty" $ do
      it "From Saturday to Friday 6 days" $ daysToParty Saturday `shouldBe` 6
      it "from Friday to Friday 0 days" $ daysToParty Friday `shouldBe` 0
      it "from Monday to Friday 4 days" $ daysToParty Monday `shouldBe` 4

    describe "num + num" $ do
      it "0 + 0" $ Z + Z `shouldBe` Z
      it "0 + 3" $ Z + S (S (S Z)) `shouldBe` S (S (S Z))
      it "10 + 1" $ 10 + S Z `shouldBe` S (S (S (S (S (S (S (S (S (S (S Z))))))))))

    describe "num * num" $ do
      it "0 * 0" $ Z * Z `shouldBe` Z
      it "2 * 3" $ (S (S Z)) * S (S (S Z)) `shouldBe` S (S (S (S (S (S Z)))))
      it "10 * 1" $ 10 * S Z `shouldBe` S (S (S (S (S (S (S (S (S (S Z)))))))))
      it "10 * 4" $ 10 * (S (S (S (S Z)))) `shouldBe` S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S Z)))))))))))))))))))))))))))))))))))))))

    describe "num - num" $ do
      it "0 - 0" $ Z - Z `shouldBe` Z
      it "3 - 1" $ S (S (S Z)) - S Z `shouldBe` S (S Z)
      it "10 - 1" $ 10 - S Z `shouldBe` S (S (S (S (S (S (S (S (S Z))))))))

    describe "toInteger" $ do
      it "Z = 0" $ natToInteger Z `shouldBe` 0
      it "S (S (S Z)) = 3" $ natToInteger (S (S (S Z))) `shouldBe` 3
      it "S (S (S (S (S (S (S (S (S Z)))))))) = 9" $ natToInteger (S (S (S (S (S (S (S (S (S Z))))))))) `shouldBe` 9

    describe "isEqual" $ do
      it "Z = Z" $ Z == Z `shouldBe` True
      it "S (S (S Z)) = S (S (S Z))" $ S (S (S Z)) == S (S (S Z)) `shouldBe` True
      it "Z = S Z" $ Z == S Z `shouldBe` False
      it "S Z = S (S (S (S (S (S (S (S (S Z))))))))" $ S (S (S (S (S (S (S (S (S Z)))))))) == Z `shouldBe` False

    describe "compare" $ do
      it "Z < Z" $ Z < Z `shouldBe` False
      it "S (S (S Z)) > S (S (S Z))" $ S (S (S Z)) > S (S (S Z)) `shouldBe` False
      it "Z < S Z" $ Z < S Z `shouldBe` True
      it "S Z <= S (S (S (S (S (S (S (S (S Z))))))))" $ (S Z) <=  S (S (S (S (S (S (S (S (S Z)))))))) `shouldBe` True

    describe "natIsEven" $ do
      it "Z natIsEven = True " $ natIsEven Z `shouldBe` True
      it "S (S (S Z)) natIsEven" $ natIsEven (S (S (S Z))) `shouldBe` False
      it "natIsEven S Z" $ natIsEven (S Z) `shouldBe` False
      it "natIsEven S (S (S (S (S (S (S (S Z))))))) = False" $ natIsEven (S (S (S (S (S (S (S (S Z)))))))) `shouldBe` True

    describe "div" $ do
      it "S Z div S Z" $ (S Z) `natDiv` (S Z) `shouldBe` (S Z)
      it "S (S (S Z)) div S (S Z)" $ (S (S (S Z))) `natDiv` (S (S (S Z))) `shouldBe` (S Z)
      it "Z div S Z" $ Z `natDiv` (S Z) `shouldBe` Z
      it "S (S (S (S (S (S (S (S (S Z)))))))) div S (S Z)" $ (S (S (S (S (S (S (S (S (S Z))))))))) `natDiv` (S (S Z)) `shouldBe` (S (S (S (S Z))))

    describe "mod" $ do
      it "S Z mod S Z" $ (S Z) `natMod` (S Z) `shouldBe` Z
      it "S (S (S Z)) mod S (S Z)" $ (S (S (S Z))) `natMod` (S (S Z)) `shouldBe` S Z
      it "Z mod S Z" $ Z `natMod` (S Z) `shouldBe` Z
      it "S (S (S (S (S (S (S (S (S Z)))))))) div S (S Z)" $ (S (S (S (S (S (S (S (S (S Z))))))))) `natMod` (S (S Z)) `shouldBe` S Z

    describe "binaryTree isEmpty" $ do
      it "Node (3 :| []) Leaf (Node (2 :| []) Leaf (Node (1 :| []) Leaf Leaf)) is non empty" $ isEmpty (fromList [1, 2, 3]) `shouldBe` False
      it "Leaf" $ isEmpty Leaf `shouldBe` True

    describe "binaryTree size" $ do
      it "Node (3 :| []) Leaf (Node (2 :| []) Leaf (Node (1 :| []) Leaf Leaf)) Size = 3" $ binaryTreeSize  (fromList [1, 2, 3]) `shouldBe` 3
      it "Leaf size = 0" $ binaryTreeSize Leaf `shouldBe` 0
      it "Node (1 :| []) Leaf Leaf" $ binaryTreeSize (fromList [1]) `shouldBe` 1

    describe "binaryTree contains" $ do
      it "Node (3 :| []) Leaf (Node (2 :| []) Leaf (Node (1 :| []) Leaf Leaf)) comtains 2 = True" $ contains (fromList [1, 2, 3]) 3 `shouldBe` True
      it "Node (3 :| []) Leaf (Node (2 :| []) Leaf (Node (1 :| []) Leaf Leaf)) comtains 4 = False" $ contains (fromList [1, 2, 3]) 4 `shouldBe` False
      it "Leaf size contains 1" $ contains Leaf 1 `shouldBe` False

    describe "binaryTree foldr" $ do
      it "foldr (*) 2 (fromList [1, 2, 3, 4, 5, 1, 1, 1, 1]) = 8" $ foldr (+) 2 (fromList [1, 2, 3]) `shouldBe` 8
      it "foldr (*) 2 (fromList [1, 2, 3, 4, 5, 1, 1, 1, 1]) = 240" $ foldr (*) 2 (fromList [1, 2, 3, 4, 5, 1, 1, 1, 1]) `shouldBe` 240

    describe "binaryTree foldMap" $ do
      it "foldMap (: [1, 2, 3]) (fromList [1, 2, 3]) = [3,1,2,3,2,1,2,3,1,1,2,3]" $ foldMap (: [1, 2, 3]) (fromList [1, 2, 3]) `shouldBe` [3,1,2,3,2,1,2,3,1,1,2,3]
      it "foldMap (: []) (fromList [1, 2, 3]) = [3,2,1]" $ foldMap (: []) (fromList [1, 2, 3]) `shouldBe` [3,2,1]

