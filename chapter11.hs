module Lib where

cHead :: [a] -> Maybe a
cHead [] = Nothing
cHead (x:_) = Just x

cTail :: [a] -> Maybe [a]
cTail [] = Nothing
cTail (_:xs) = Just xs

cFilter :: (a -> Bool) -> [a] -> [a]
cFilter f (x:xs) =
  if f x
    then x : cFilter f xs
    else cFilter f xs
cFilter _ [] = []

cFoldl :: (a -> b -> a) -> a -> [b] -> a
cFoldl _ initial [] = initial
cFoldl f initial (x:xs) = cFoldl f (f initial x) xs
