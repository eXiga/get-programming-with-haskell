{-# LANGUAGE ScopedTypeVariables #-}

module Lib where

sumSquareOrSquareSum :: (Num a, Ord a) => a -> a -> a
sumSquareOrSquareSum x y =
  if sumSquare > squareSum
    then sumSquare
    else squareSum
  where
    sumSquare = x ^ 2 + y ^ 2
    squareSum = (x + y) ^ 2

-- As I understand, GHC doesn't know how to compare 2 vars within lambda w\o type binding
-- This is why I had to add ScopedTypeVariables pragma. For sure, I understand, that it will
-- be much more easier to rewrite this stuff w\o using lambdas, but I want to follow the book.
body =
  (\(sumSquare :: Int) (squareSum :: Int) ->
     if sumSquare > squareSum
       then sumSquare
       else squareSum)

-- I hope this fiesta is over, ghc-mod and linter are so mad at me lol.
optimizedSumSquareOrSquareSum :: Int -> Int -> Int
optimizedSumSquareOrSquareSum x y = body (x ^ 2 + y ^ 2) ((x + y) ^ 2)

letBinding :: (Num a, Ord a) => a -> a -> a
letBinding x y =
  let sumSquare = (x ^ 2 + y ^ 2)
      squareSum = ((x + y) ^ 2)
  in if sumSquare > squareSum
       then sumSquare
       else squareSum

counter x = (\x -> x + 1) ((\x -> x + 1) ((\x -> x) x))
