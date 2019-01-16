module Chapter6 where

cRepeat :: a -> [a]
cRepeat n = cycle [n]

subseq :: Int -> Int -> [a] -> [a]
subseq startIndex endIndex aList =
  take (endIndex - startIndex) (drop startIndex aList)

inFirstHalf :: (Eq a) => a -> [a] -> Bool
inFirstHalf n aList = n `elem` take half aList
  where
    half = length aList `div` 2
