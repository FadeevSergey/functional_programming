{-# LANGUAGE InstanceSigs #-}

module Block3 where

import Control.Arrow (first)

-- Block 3
-- Task 1

data Parser s a = Parser { runParser :: [s] -> Maybe (a, [s]) }

instance Functor (Parser s) where
  fmap :: (a -> b) -> Parser s a -> Parser s b
  fmap f (Parser x) = Parser (fmap (first f) . x)

-- Block 3
-- Task 2

ok :: Parser s ()
ok = Parser (\s -> Just ((), s))

eof :: Parser s ()
eof = Parser $ \s ->
  case s of
    [] -> Just ((), s)
    _  -> Nothing

satisfy :: (s -> Bool) -> Parser s s
satisfy p = Parser $ \s ->
  case s of
    []     -> Nothing
    (x:xs) -> if p x then Just (x, xs) else Nothing

element :: Eq s => s -> Parser s s
element x = satisfy (== x)