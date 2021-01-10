module Main where

import Test.Tasty (defaultMain, testGroup)

import qualified Block1And2Test (test)
import qualified Block3Test (test)

main :: IO ()
main = do
  testBlock1And2 <- Block1And2Test.test
  testBlock3 <- Block3Test.test
  defaultMain $ testGroup "Tests" [testBlock1And2, testBlock3]
