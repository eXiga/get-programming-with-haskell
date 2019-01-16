module Chapter21 where

import qualified Data.Map as Map

-- ex. 21.1
names :: Map.Map Int String
names = Map.fromList [(1, "John")]

helloPerson :: String -> String
helloPerson name = "Hello, " ++ name ++ "!"

maybeMain :: Maybe String
maybeMain = do
  name <- Map.lookup 1 names
  return $ helloPerson name

-- ex. 21.2
fibonacci :: Int -> Integer
fibonacci n = fs !! n
  where
    fs = 0 : 1 : zipWith (+) fs (tail fs)

fibMain :: IO ()
fibMain = do
  putStrLn "Please, enter the number"
  number <- getLine
  print $ fibonacci $ read number
