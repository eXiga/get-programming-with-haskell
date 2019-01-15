module Chapter22 where

import Data.Char (digitToInt, isSpace)

-- Quick check 1
qc1 :: IO ()
qc1 = do
  vals <- mapM (const getLine) [1 .. 3]
  mapM_ putStrLn vals

-- Quick check 2
qc2 :: Monad m => Int -> m a -> m [a]
qc2 n m = mapM (const m) [1 .. n]

-- Quick check 3
qc3 :: IO ()
qc3 = do
  userInput <- getContents
  putStrLn $ reverse userInput

-- Quick check 4
toInts :: String -> [Int]
toInts = map read . lines

qc4 :: IO ()
qc4 = do
  userInput <- getContents
  print $ sum $ map (^ 2) $ toInts userInput

-- Final question 1
parseOperator :: (Num a) => Char -> a -> a -> a
parseOperator '+' = (+)
parseOperator '*' = (*)
parseOperator _ = error "Unknown operator"

calculate :: String -> Int
calculate e = perform $ filter (not . isSpace) e
  where
    perform (v1:op:v2:_) = parseOperator op (digitToInt v1) (digitToInt v2)

task1 :: IO ()
task1 = do
  userInput <- getContents
  print $ map calculate $ lines userInput

-- Final question 2
quotes :: [String]
quotes = ["Quote 1", "Quote 2", "Quote 3", "Quote 4", "Quote 5"]

findQuote :: [String] -> [String]
findQuote [] = []
findQuote ("n":_) = []
findQuote (x:xs) = quote : findQuote xs
  where
    quote
      | read x `elem` [1 .. length quotes] = quotes !! (read x - 1)
      | otherwise = "Incorrect number of quote"

task2 :: IO ()
task2 = do
  userInput <- getContents
  mapM_ putStrLn $ findQuote $ lines userInput
