module Main where

import Test.Tasty (defaultMain, testGroup)

import qualified Block1Test (test)
import qualified Block2Test (test)
import qualified Block3Test (test)

main :: IO ()
main = do
  testBlock1 <- Block1Test.test
  testBlock2 <- Block2Test.test
  testBlock3 <- Block3Test.test
  defaultMain $ testGroup "Tests" [testBlock1, testBlock2, testBlock3]