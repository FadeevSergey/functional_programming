{-# LANGUAGE BangPatterns #-}

module Task1
    ( Point(..)
    , plus
    , minus
    , scalarProduct
    , crossProduct
    , perimeter
    , doubleArea
    ) where

data Point = Point { x :: Int, y :: Int} deriving (Show)

plus :: Point -> Point -> Point
plus (Point x1 y1) (Point x2 y2) = Point (x1 + x2) (y1 + y2)

minus :: Point -> Point -> Point
minus (Point x1 y1) (Point x2 y2) = Point (x1 - x2) (y1 - y2)

scalarProduct :: Point -> Point -> Int
scalarProduct (Point x1 y1) (Point x2 y2) = x1 * x2 + y1 * y2

crossProduct :: Point -> Point -> Int
crossProduct (Point x1 y1) (Point x2 y2) = x1 * y2 - x2 * y1

vectorLen :: Point -> Point -> Double
vectorLen (Point x1 y1) (Point x2 y2) = sqrt . fromIntegral $ (sqr (x2 - x1) + sqr (y2 - y1))
  where
    sqr :: Int -> Int
    sqr a = a * a

perimeter :: [Point] -> Double
perimeter []                         = error "Imvalid input poligon"
perimeter [_]                        = error "Imvalid input poligon"
perimeter (startPoint:point:arrTail) = perimeterAcc (vectorLen startPoint point) startPoint (point:arrTail)
  where
    perimeterAcc :: Double -> Point -> [Point] -> Double
    perimeterAcc !prevLen _             []                        = prevLen
    perimeterAcc !prevLen startPointAcc [pointAcc]                = prevLen + vectorLen startPointAcc pointAcc
    perimeterAcc !prevLen startPointAcc (point1:(point2:tailAcc)) =
      prevLen + perimeterAcc (vectorLen point1 point2) startPointAcc (point2:tailAcc)

doubleArea :: [Point] -> Int
doubleArea []                         = error "Imvalid input poligon"
doubleArea [_]                        = error "Imvalid input poligon"
doubleArea (startPoint:point:arrTail) = doubleAreaAcc (crossProduct startPoint point) startPoint (point:arrTail)
  where
    doubleAreaAcc :: Int -> Point -> [Point] -> Int
    doubleAreaAcc !prev _             []                        = prev
    doubleAreaAcc !prev startPointAcc [pointAcc]                = prev + crossProduct pointAcc startPointAcc
    doubleAreaAcc !prev startPointAcc (point1:(point2:tailAcc)) =
      doubleAreaAcc (prev + crossProduct point1 point2) startPointAcc (point2:tailAcc)
