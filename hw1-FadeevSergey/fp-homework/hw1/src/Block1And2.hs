{-# LANGUAGE InstanceSigs #-}
module Block1And2 where

import Data.Maybe
import Data.List.NonEmpty( NonEmpty((:|)), (<|) )

-- Block 1
-- Task 1
data Days = Monday
          | Tuesday
          | Wednesday 
          | Thursday
          | Friday
          | Saturday
          | Sunday deriving (Show) 

instance Eq Days where
    (==) :: Days -> Days -> Bool 
    firstDay == secondDay = show firstDay == show secondDay 

mapOfDays = [(Monday, 1),
            (Tuesday, 2),
            (Wednesday, 3),
            (Thursday, 4),
            (Friday, 5),
            (Saturday, 6),
            (Sunday, 7)]  

numberToDay :: Int -> Days
numberToDay number = fromJust (lookup number (map swap mapOfDays))

dayToNumber :: Days -> Int
dayToNumber day = fromJust (lookup day mapOfDays)

swap :: (Days, Int) -> (Int, Days)
swap (x,y) = (y,x) 

nextDay :: Days -> Days
nextDay day = afterDays day 1

afterDays :: Days -> Int -> Days
afterDays day 0 = day
afterDays day number = numberToDay (rem ((dayToNumber day) + number) 7)

isWeekend :: Days -> Bool
isWeekend day = (dayToNumber day) >= 6

daysToParty :: Days -> Int
daysToParty day = rem (7 + delta) 7 where
    delta = dayToNumber Friday - dayToNumber day

-- Task 2
data Nat = Z | S Nat deriving (Show)

instance Num Nat where
    (+) :: Nat -> Nat -> Nat
    Z + nat = nat
    (S firstNat) + secondNat = S (firstNat + secondNat)

    (*) :: Nat -> Nat -> Nat
    Z * nat = Z
    (S firstNat) * secondNat = (firstNat * secondNat) + secondNat

    (-) :: Nat -> Nat -> Nat
    Z - Z = Z
    Z - (S nat) = error "The result of subtraction is not a natural number"
    (S nat) - Z = S nat
    (S firstNat) - (S secondNat) = firstNat - secondNat

    fromInteger :: Integer -> Nat
    fromInteger number | number < 0  = error "Can't converte negative Integer to natural number"
                       | number == 0 = Z
                       | otherwise   = S (fromInteger (number - 1))

    abs :: Nat -> Nat
    abs nat = nat

    signum :: Nat -> Nat
    signum nat = 1

instance Eq Nat where
    (==) :: Nat -> Nat -> Bool
    Z == Z = True
    Z == (S nat) = False
    (S nat) == Z = False
    (S firstNat) == (S secondNat) = firstNat == secondNat

    (/=) :: Nat -> Nat -> Bool
    firstNat /= secondNat = not (firstNat == secondNat)

instance Ord Nat where
    compare :: Nat -> Nat -> Ordering
    compare Z Z = EQ
    compare nat Z = GT
    compare Z nat = LT
    compare (S firstNat) (S secondNat) = compare firstNat secondNat

    (<) :: Nat -> Nat -> Bool
    Z < Z = False
    Z < S (nat) = True
    S (nat) < Z = False
    S (firstNat) < S (secondNat) = firstNat < secondNat

    (<=) :: Nat -> Nat -> Bool
    firstNat <= secondNat = (firstNat < secondNat) || (firstNat == secondNat)

    (>) :: Nat -> Nat -> Bool
    firstNat > secondNat = not ((firstNat == secondNat) || (firstNat < secondNat))

    (>=) :: Nat -> Nat -> Bool
    firstNat >= secondNat = not (firstNat < secondNat)

natToInteger :: Nat -> Integer 
natToInteger Z = 0
natToInteger (S nat) = 1 + natToInteger nat

natIsEven :: Nat -> Bool
natIsEven Z = True
natIsEven (S (Z)) = False
natIsEven (S (S (nat))) = natIsEven nat

natDiv :: Nat -> Nat -> Nat 
natDiv nat Z = error "Divide by zero"
natDiv firstNat secondNat | secondNat == Z = error "Divide by zero"
                          | firstNat < secondNat = Z
                          | otherwise = S (natDiv (firstNat - secondNat) secondNat)

natMod :: Nat -> Nat -> Nat
natMod firstNat secondNat | secondNat == Z = error "Divide by zero"
                          | (firstNat < secondNat) = firstNat
                          | otherwise = natMod (firstNat - secondNat) secondNat 

-- Task 3
data BinaryTree a = Leaf | Node (NonEmpty a) (BinaryTree a) (BinaryTree a) deriving (Show)

isEmpty :: BinaryTree a -> Bool
isEmpty Leaf = True
isEmpty (Node _ _ _) = False
 
binaryTreeSize :: BinaryTree a -> Int
binaryTreeSize Leaf = 0
binaryTreeSize (Node elements leftTree rightTree) = length elements +
                                                  binaryTreeSize leftTree +
                                                  binaryTreeSize rightTree

contains :: Ord a => BinaryTree a -> a -> Bool 
contains Leaf element = False 
contains (Node (x :| xs) leftTree rightTree) element | x == element = True
                                                 | x < element = contains rightTree element
                                                 | otherwise = contains leftTree element
 
insert :: Ord a => BinaryTree a -> a -> BinaryTree a     
insert Leaf element = (Node (element :| []) Leaf Leaf)
insert (Node (x :| xs) leftTree rightTree) element | x == element = (Node (element :| x : xs) leftTree rightTree)
                                                   | x < element  = (Node (x :| xs) (insert leftTree element) rightTree)
                                                   | x > element  = (Node (x :| xs) leftTree (insert rightTree element))
 
fromList :: Ord a => [a] -> BinaryTree a 
fromList [] = Leaf
fromList (x : xs) = insert (fromList xs) x

erase :: Ord a => BinaryTree a -> a -> BinaryTree a  
erase Leaf _ = Leaf
erase (Node (x :| xs) leftTree rightTree) element | x == element = eraseFromNode (Node (x :| xs) leftTree rightTree) element
                                                  | x < element  = (Node (x :| xs) (erase leftTree element) rightTree)
                                                  | x > element  = (Node (x :| xs) leftTree (erase rightTree element))

eraseFromNode :: Ord a => BinaryTree a -> a -> BinaryTree a 
eraseFromNode (Node (x :| []) leftTree rightTree) = undefined 
eraseFromNode (Node (x :| y : xs) leftTree rightTree) = undefined
--eraseFromNode (Node (x :| y : xs) leftTree rightTree) = (Node (x :| xs) leftTree rightTree)

-- Block 2
-- Task 1

instance Foldable BinaryTree where
    foldr :: (a -> b -> b) -> b -> BinaryTree a -> b
    foldr function newElement Leaf = newElement
    -- leftTree ... list ... rightTree newElement
    -- 1) foldr rightTree newElement = res1
    -- 2) foldr res list = res2
    -- 3) foldr res2 leftTree 
    -- -1) (foldr function newElement rightTree) = res1
    -- -2) (foldr (foldr function newElement rightTree) list) = res2
    foldr function newElement (Node list leftTree rightTree) = foldr function (foldr function (foldr function newElement rightTree) list) leftTree

    foldMap :: Monoid m => (a -> m) -> BinaryTree a -> m   
    foldMap function Leaf = mempty 
    -- leftTree ... list .. rightTree     
    -- leftTree <> list <> rightTree    
    foldMap function (Node list leftTree rightTree) = (foldMap function leftTree) <>
                                                      (foldMap function list) <>
                                                      (foldMap function rightTree)

-- Task 2

splitOn :: Eq a => a -> [a] -> NonEmpty [a]
splitOn separator = foldr (getList separator) ([] :| [])

getList :: Eq a => a -> a -> NonEmpty [a] -> NonEmpty [a]
getList separator newSumbol (x :| xs) | newSumbol == separator = ([] :| (x : xs))
                                    | otherwise = ((newSumbol : x) :| xs)



