module Lib where

cTake :: Int -> [a] -> [a]
cTake n =
  foldl
    (\acc x ->
       if length acc == n
         then acc
         else acc ++ [x])
    []

cDrop :: Int -> [a] -> [a]
cDrop n xs =
  foldr
    (\x acc ->
       if length acc == length xs - n
         then acc
         else x : acc)
    []
    xs

cLength :: [a] -> Int
cLength [] = 0
cLength (_:xs) = 1 + cLength xs

rcTake :: Int -> [a] -> [a]
rcTake 0 _ = []
rcTake _ [] = []
rcTake n (x:xs) = x : rest
  where
    rest = rcTake (n - 1) xs

cCycle :: [a] -> [a]
cCycle [] = []
cCycle (x:xs) = x : cCycle (xs ++ [x])

cReverse :: [a] -> [a]
cReverse [] = []
cReverse (x:xs) = cReverse xs ++ [x]

fibonacci :: (Num a, Eq a) => a -> a
fibonacci = fibIter 1 0
  where
    fibIter :: (Num a, Eq a) => a -> a -> a -> a
    fibIter a b count
      | count == 0 = b
      | otherwise = fibIter (a + b) a (count - 1)
