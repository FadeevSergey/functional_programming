{-# LANGUAGE BlockArguments #-}

module Block3Test
  ( test
  ) where

import Block3
import Test.Tasty (TestTree)
import Data.Char  (isUpper)
import Test.Tasty.Hspec (describe, it, shouldBe, testSpec)

test :: IO TestTree
test =
  testSpec "Parsers" $ do
    describe "Ok" $ do
      it "ok \"Hello!\"" $ runParser ok "Hello!" `shouldBe` Just ((),"Hello!")
      it "ok \"Hel-1-1-1-1--1-1       lo!\"" $ runParser ok "Hel-1-1-1-1--1-1       lo!" `shouldBe` Just ((),"Hel-1-1-1-1--1-1       lo!")

    describe "Eof" $ do
      it "Eof \"\"" $ runParser eof "" `shouldBe` Just ((),"")
      it "Eof \"Not empty\"" $ runParser eof "Not empty" `shouldBe` Nothing

    describe "Satisfy" $ do
      it "Satisfy is upper hello" $ runParser ( satisfy isUpper) "hello" `shouldBe` Nothing
      it "Satisfy is upper HELLO" $ runParser ( satisfy isUpper) "HELLO" `shouldBe` Just ('H',"ELLO")

    describe "Element" $ do
      it "Element 'h' from \"HELLO\"" $ runParser (element 'h') "HELLO" `shouldBe` Nothing
      it "Element 'H' from \"HELLO\"" $ runParser (element 'H') "HELLO" `shouldBe` Just ('H',"ELLO")
