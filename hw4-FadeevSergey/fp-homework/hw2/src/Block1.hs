{-# LANGUAGE InstanceSigs #-}

module Block1 where

import Text.Read (readMaybe)

-- Block 1
-- Task 1

stringSum :: String -> Maybe Int
stringSum str | str == "" = Nothing
              | otherwise = (fmap sum . traverse readMaybe . words) str

-- Block 1
-- Task 2

data Tree a
  = Branch (Tree a) (Tree a)
  | Leaf a

-- only for testing
instance (Show a) => Show (Tree a) where
  show (Leaf a)     = "Leaf: " ++ show a
  show (Branch l r) = "(Left: " ++ show l ++ ") " ++ "(Right: " ++ show r ++ ")"

instance Functor Tree where
  fmap :: (a -> b) -> Tree a -> Tree b
  fmap f (Leaf l)     = Leaf (f l)
  fmap f (Branch l r) = Branch (fmap f l) (fmap f r)

instance Applicative Tree where
  pure :: a -> Tree a
  pure a = Leaf a

  (<*>) :: Tree (a -> b) -> Tree a -> Tree b
  (Leaf l)       <*> x = fmap l x
  (Branch lF rF) <*> x = Branch (lF <*> x) (rF <*> x)

instance Foldable Tree where
  foldMap :: Monoid m => (a -> m) -> Tree a -> m
  foldMap f (Leaf l)     = f l
  foldMap f (Branch l r) = (foldMap f l) `mappend` (foldMap f r)

instance Traversable Tree where
  traverse :: Applicative f => (a -> f b) -> Tree a -> f (Tree b)
  traverse f (Leaf l)     = Leaf <$> f l
  traverse f (Branch l r) = Branch <$> traverse f l <*> traverse f r

-- Block 1
-- Task 3

-- deriving show only for testing
data NonEmpty a = a :| [a] deriving (Show)

concatNonEmpty :: NonEmpty a -> NonEmpty a -> NonEmpty a
concatNonEmpty (a :| as) (b :| bs) = a :| (as ++ b : bs)

instance Functor NonEmpty where
  fmap :: (a -> b) -> NonEmpty a -> NonEmpty b
  fmap f (x :| xs) = (f x) :| (fmap f xs)

instance Applicative NonEmpty where
  pure :: a -> NonEmpty a
  pure x = x :| []

  (<*>) :: NonEmpty (a -> b) -> NonEmpty a -> NonEmpty b
  (f :| fs) <*> (x :| xs) = (f x) :| ((fmap f xs) ++ (fs <*> xs))

instance Foldable NonEmpty where
  foldMap :: Monoid m => (a -> m) -> NonEmpty a -> m
  foldMap f (x :| xs) = (f x) <> (foldMap f xs)

instance Traversable NonEmpty where
  traverse :: Applicative f => (a -> f b) -> NonEmpty a -> f (NonEmpty b)
  traverse f (x :| xs) = fmap (:|) (f x) <*> traverse f xs

instance Monad NonEmpty where
  (>>=) :: NonEmpty a -> (a -> NonEmpty b) -> NonEmpty b
  (x :| xs) >>= f = foldl concatNonEmpty (f x) (map f xs)