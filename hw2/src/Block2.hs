{-# LANGUAGE InstanceSigs #-}

module Block2 where

import Control.Monad.State (State, put, get, evalState)

-- Block 2
-- Task 1

data ArithmeticError = DivisionByZeroError | NegativePowError deriving (Show, Eq)

data Expr = Constant Int | Operation Operator Expr Expr

data Operator = Plus | Minus | Mul | Div | Pow deriving (Eq)

eval :: Expr -> Either ArithmeticError Int
eval (Constant a) = Right a
eval (Operation operator firstExpr secondExpr) = eval firstExpr >>= (\x -> eval secondExpr >>= (eitherEval operator x))

eitherEval :: Operator -> Int -> Int -> Either ArithmeticError Int
eitherEval operator first second | operator == Plus = Right(first + second)
                                 | operator == Minus = Right(first - second)
                                 | operator == Mul = Right(first * second)
                                 | operator == Div = if second /= 0 then Right(div first second) else Left(DivisionByZeroError)
                                 | operator == Pow = if second >= 0 then Right(first ^ second) else Left(NegativePowError)

-- Block 2
-- Task 2

simpleMovingAverage :: Int -> Double -> State [Double] Double
simpleMovingAverage n newElement = do
  prevList <- get
  let curList        = prevList ++ [newElement]
  put curList
  let subArrayLength = if length curList < n then length curList else n
  let averageOfNDays = (sumLastN n curList) / fromIntegral(subArrayLength)
  return averageOfNDays

sumLastN :: Int -> [Double] -> Double
sumLastN n list | n >= length list = sum list
                | otherwise        = sum (drop (length list - n) list)

moving :: Int -> [Double] -> [Double]
moving period list = evalState (mapM (simpleMovingAverage period) list) []