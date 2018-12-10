module Lib where

import Data.Char (isSpace, toLower)

cMap :: (a -> b) -> [a] -> [b]
cMap _ [] = []
cMap f (x:xs) = f x : cMap f xs

remove :: (a -> Bool) -> [a] -> [a]
remove _ [] = []
remove f (x:xs) =
  if f x
    then remove f xs
    else x : remove f xs

cProduct :: (Num a) => [a] -> a
cProduct = foldl (*) 1

cElem :: (Eq a) => a -> [a] -> Bool
cElem item list = length (filter (== item) list) /= 0

isPalindrome :: String -> Bool
isPalindrome word = preparedWord == reverse preparedWord
  where
    preparedWord = map toLower (filter (not . isSpace) word)

harmonic :: (Fractional a, Enum a) => Int -> a
harmonic n = sum (take n [1 / x | x <- [1 ..]])
