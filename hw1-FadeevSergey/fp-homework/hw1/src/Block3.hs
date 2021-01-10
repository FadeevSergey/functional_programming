{-# LANGUAGE InstanceSigs #-}
module Block3 where

-- Block 3
-- Task 1

maybeConcat :: [Maybe[a]] -> [a]
maybeConcat = foldr concatMaybe []

concatMaybe :: Maybe [a] -> [a] -> [a]
concatMaybe Nothing list = list
concatMaybe (Just firstList) secondList = firstList ++ secondList 
 
-- Block 3 
-- Task 2

data NonEmpty a = a :| [a] deriving (Show)

instance Semigroup (NonEmpty a) where 
    (<>) :: NonEmpty a -> NonEmpty a -> NonEmpty a
    -- x ... xs ... y ... ys
    -- x ++ xs ++ y ++ ys
    -- x :| (xs ++ y ++ ys)
    -- x :| (xs ++ (y ++ ys))
    -- x :| (xs ++ (y : ys))   
    (x :| xs) <> (y :| ys) = (x :| (xs ++ (y : ys)))

data ThisOrThat a b = This a | That b | Both a b deriving (Show, Eq) -- eq onle for tests

instance (Semigroup a, Semigroup b) => Semigroup (ThisOrThat a b) where
  (<>) :: ThisOrThat a b -> ThisOrThat a b -> ThisOrThat a b
  This a <> This b = This (a <> b)
  This a <> That b = Both a b
  That a <> This b = Both b a
  That a <> That b = That (a <> b)
  This a <> Both firstB secondB = Both (a <> firstB) secondB
  That a <> Both firstB secondB = Both firstB (a <> secondB)
  Both firstA secondA <> This b = Both (firstA <> b) secondA
  Both firstA secondA <> That b = Both firstA (secondA <> b)
  Both firstA secondA <> Both firstB secondB = Both (firstA <> firstB) (secondA <> secondB)



