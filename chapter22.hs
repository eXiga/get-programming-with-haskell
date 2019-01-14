--module Chapter22 where
import Data.Char (digitToInt, isSpace)
import System.Environment

qc1 :: IO ()
qc1 = do
  vals <- mapM (const getLine) [1 .. 3]
  mapM_ putStrLn vals

qc2 :: Monad m => Int -> m a -> m [a]
qc2 n m = mapM (const m) [1 .. n]

qc3 :: IO ()
qc3 = do
  userInput <- getContents
  putStrLn $ reverse userInput

toInts :: String -> [Int]
toInts = map read . lines

qc4 :: IO ()
qc4 = do
  userInput <- getContents
  print $ sum $ map (^ 2) $ toInts userInput

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

quotes :: [String]
quotes = ["Quote 1", "Quote 2", "Quote 3", "Quote 4", "Quote 5"]

findQuote :: [String] -> [String]
findQuote [] = []
findQuote ("n":_) = []
findQuote (x:xs) = quote : findQuote xs
  where
    quote = quotes !! (read x - 1)

main :: IO ()
main = do
  userInput <- getContents
  mapM_ putStrLn $ findQuote $ lines userInput
