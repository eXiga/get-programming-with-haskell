{-# LANGUAGE OverloadedStrings #-}

module Chapter23 where

import Data.Semigroup
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TLIO

firstWord :: String
firstWord = "firstWord"

secondWord :: T.Text
secondWord = T.pack firstWord

thirdWord :: String
thirdWord = T.unpack secondWord

-- Quick check 1 
fourthWord :: T.Text
fourthWord = T.pack thirdWord

-- Quick check 2
cLines :: T.Text -> [T.Text]
cLines = T.splitOn "\n"

cUnlines :: [T.Text] -> T.Text
cUnlines = T.intercalate " "

exampleText :: T.Text
exampleText = "This\nis\njust\nexample"

-- Working with unicode
sentence :: T.Text
sentence = "a dog walking dogs"

unicodeSentence :: T.Text
unicodeSentence =
  "श्रेयान्स्वधर्मो विगुणः परधर्मात्स्वनुष्ठितात्।स्वधर्मे निधनं श्रेयः परधर्मो"

query :: T.Text
query = "dog"

unicodeQuery :: T.Text
unicodeQuery = "धर्म"

highlight :: T.Text -> T.Text -> T.Text
highlight q t = T.intercalate highlighted pieces
  where
    pieces = T.splitOn q t
    highlighted = mconcat ["{", q, "}"]

-- Final question 1
helloPerson :: T.Text -> T.Text
helloPerson name = "Hello," <> " " <> name <> "!"

greeter :: IO ()
greeter = do
  TIO.putStrLn "Hello! What's your name?"
  name <- TIO.getLine
  let statement = helloPerson name
  TIO.putStrLn statement

-- Final question 2
toInts :: TL.Text -> [Int]
toInts = map (read . TL.unpack) . TL.lines

inputSum :: IO ()
inputSum = do
  userInput <- TLIO.getContents
  let numbers = toInts userInput
  print $ sum numbers
