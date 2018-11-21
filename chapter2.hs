module Lib where

calcChange :: (Num a, Ord a) => a -> a -> a
calcChange owed given =
  if change > 0
    then change
    else 0
  where
    change = given - owed

doublePlustTwo :: Num a => a -> a
doublePlustTwo x = doubleX + 2
  where
    doubleX = 2 * x

inc :: Num a => a -> a
inc x = x + 1

double :: Num a => a -> a
double x = 2 * x

square :: Num a => a -> a
square x = x * x

calculate :: Integral a => a -> a
calculate x =
  if even x
    then x - 2
    else (3 * x) - 1
