{-# LANGUAGE BlockArguments #-}

module Block2Test
  ( test
  ) where

import Block2
import Test.Tasty (TestTree)
import Test.Tasty.Hspec (describe, it, shouldBe, testSpec)

test :: IO TestTree
test =
  testSpec "HW2 Block2" $ do
    describe "Expression plus" $ do
      it "1 + 0" $ eval (Operation Plus (Constant 1) (Constant 0)) `shouldBe` Right 1
      it "-1 + 0" $ eval (Operation Plus (Constant (-1)) (Constant 2)) `shouldBe` Right 1
      it "1 + 0" $ eval (Operation Plus (Constant 1) (Constant 0)) `shouldBe` Right 1
      it "-1 + (10 + 10)" $ eval (Operation Plus (Constant (-1)) (Operation Plus (Constant (10)) (Constant 10))) `shouldBe` Right 19
      it "-1 + (10 - 9)" $ eval (Operation Plus (Constant (-1)) (Operation Minus (Constant (10)) (Constant 9))) `shouldBe` Right 0
      it "-1 + (10 * 10)" $ eval (Operation Plus (Constant (-1)) (Operation Mul (Constant (10)) (Constant 10))) `shouldBe` Right 99

    describe "Expression minus" $ do
      it "1 - 0" $ eval (Operation Minus (Constant 1) (Constant 0)) `shouldBe` Right 1
      it "-1 - 2" $ eval (Operation Minus (Constant (-1)) (Constant 2)) `shouldBe` Right (-3)
      it "-1 - (10 + 10)" $ eval (Operation Minus (Constant (-1)) (Operation Plus (Constant (10)) (Constant 10))) `shouldBe` Right (-21)
      it "-1 - (10 - 9)" $ eval (Operation Minus (Constant (-1)) (Operation Minus (Constant (10)) (Constant 9))) `shouldBe` Right (-2)
      it "-1 - (10 * 10)" $ eval (Operation Minus (Constant (-1)) (Operation Mul (Constant (10)) (Constant 10))) `shouldBe` Right (-101)

    describe "Expression multiply" $ do
      it "1 * 0" $ eval (Operation Mul (Constant 1) (Constant 0)) `shouldBe` Right 0
      it "1 * 2" $ eval (Operation Mul (Constant 1) (Constant 2)) `shouldBe` Right 2
      it "5 * (10 + 10)" $ eval (Operation Mul (Constant (5)) (Operation Plus (Constant (10)) (Constant 10))) `shouldBe` Right 100
      it "-5 * (10 - 9)" $ eval (Operation Mul (Constant (-5)) (Operation Minus (Constant (10)) (Constant 9))) `shouldBe` Right (-5)
      it "-10 * (10 * 10)" $ eval (Operation Mul (Constant (-10)) (Operation Mul (Constant (10)) (Constant 10))) `shouldBe` Right (-1000)

    describe "Expression divide" $ do
      it "Int(1 / 0)" $ eval (Operation Div (Constant 1) (Constant 0)) `shouldBe` Left DivisionByZeroError
      it "Int(1 / 2)" $ eval (Operation Div (Constant 1) (Constant 2)) `shouldBe` Right 0
      it "500 / (10 + 10)" $ eval (Operation Div (Constant (500)) (Operation Plus (Constant (10)) (Constant 10))) `shouldBe` Right 25
      it "-5 / (10 - 10)" $ eval (Operation Div (Constant (-5)) (Operation Minus (Constant (10)) (Constant 10))) `shouldBe` Left DivisionByZeroError
      it "-100 / (10 * 10)" $ eval (Operation Div (Constant (-100)) (Operation Mul (Constant (10)) (Constant 10))) `shouldBe` Right (-1)

    describe "Expression pow" $ do
      it "1 ^ 0" $ eval (Operation Pow (Constant 1) (Constant 0)) `shouldBe` Right 1
      it "1 ^ 2" $ eval (Operation Pow (Constant 1) (Constant 2)) `shouldBe` Right 1
      it "2 ^ 10" $ eval (Operation Pow (Constant 2) (Constant 10)) `shouldBe` Right 1024
      it "10 ^ -2" $ eval (Operation Pow (Constant 10) (Constant (-2))) `shouldBe` Left NegativePowError
      it "500 ^ (10 + 10)" $ eval (Operation Pow (Constant (2)) (Operation Plus (Constant (10)) (Constant 10))) `shouldBe` Right 1048576
      it "-5 ^ (10 - 20)" $ eval (Operation Pow (Constant (-5)) (Operation Minus (Constant (10)) (Constant 20))) `shouldBe` Left NegativePowError
      it "-2 ^ (11 * 1)" $ eval (Operation Pow (Constant (-2)) (Operation Mul (Constant (11)) (Constant 1))) `shouldBe` Right (-2048)

    describe "Simple moving average" $ do
      it "First test from the assignment" $ moving 4 [1, 5, 3, 8, 7, 9, 6] `shouldBe` [1.0,3.0,3.0,4.25,5.75,6.75,7.5]
      it "Second test from the assignment" $ moving 2 [1, 5, 3, 8, 7, 9, 6] `shouldBe` [1.0,3.0,4.0,5.5,7.5,8.0,7.5]


